# R/fetch_weather.R
# Fetch weather data (historical + forecast) for a park from Open-Meteo.
# Uses httr2 to call the API directly (no external R wrapper package needed).
# Returns a single tidy tibble with a `source` column ("history"/"forecast").
#
# Note on soil moisture layer naming:
#   Historical API (archive-api): ERA5-Land layers → 0_to_7cm, 7_to_28cm, ...
#   Forecast API:                 ICON/GFS layers  → 0_to_1cm, 1_to_3cm, 3_to_9cm, ...
# Both are normalised to soil_moisture_0_1 / soil_moisture_1_3 / soil_moisture_3_9
# in the output (forecast API's finer layers are used directly; historical API's
# coarser 0-7 cm layer is used for all three fine slots as the best available proxy).
#
# Depends on: setup.R, parks_config.R

# ── Internal helpers ─────────────────────────────────────────────────────────

.om_parse <- function(resp) {
  bod <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (!is.null(bod$error)) stop("Open-Meteo error: ", bod$reason)
  bod
}

# Convert a named list of equal-length vectors (one of which is "time") to tibble
.om_to_tibble <- function(lst) {
  if (is.null(lst$time)) return(tibble::tibble())
  as_tibble(lst) |>
    mutate(date = as.Date(time)) |>
    select(-time)
}

# ── Main fetch function ──────────────────────────────────────────────────────

fetch_weather <- function(
    lat              = PARK_LAT,
    lon              = PARK_LON,
    start            = WEATHER_START,
    tz               = PARK_TZ,
    forecast_days    = FORECAST_DAYS,
    cache_dir        = "data/cache",
    hist_max_age_h   = 24,   # archive-api: re-fetch at most once per day
    fc_max_age_h     = 1,    # forecast-api: re-fetch at most once per hour
    api_key          = Sys.getenv("OPEN_METEO_API_KEY", unset = "")
) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  today    <- Sys.Date()
  hist_end <- today - 10L
  past_days_fc <- as.integer(today - hist_end) + 1L

  # ── Cache paths ────────────────────────────────────────────────────────────
  key       <- sprintf("%.4f_%.4f", lat, lon)
  hist_file <- file.path(cache_dir, paste0("weather_hist_", key, ".rds"))
  fc_file   <- file.path(cache_dir, paste0("weather_fc_",   key, ".rds"))

  .cache_age_h <- function(path) {
    if (!file.exists(path)) return(Inf)
    as.numeric(difftime(Sys.time(), file.mtime(path), units = "hours"))
  }

  # Optionally add API key to a request
  .maybe_key <- function(req) {
    if (nzchar(api_key)) httr2::req_url_query(req, apikey = api_key) else req
  }

  # ── 1. HISTORICAL — archive-api.open-meteo.com (cached 24 h) ─────────────
  hist_age <- .cache_age_h(hist_file)
  if (hist_age < hist_max_age_h) {
    message(sprintf("Using cached historical data (%.0f h old).", hist_age))
    hist <- readRDS(hist_file)
  } else {
    message("Fetching historical weather (", start, " to ", hist_end, ")...")

    hist <- tryCatch({
      hist_resp <- httr2::request("https://archive-api.open-meteo.com/v1/archive") |>
        httr2::req_url_query(
          latitude   = lat,
          longitude  = lon,
          start_date = format(start),
          end_date   = format(hist_end),
          timezone   = tz,
          daily      = paste(c(
            "precipitation_sum", "temperature_2m_max", "temperature_2m_min",
            "et0_fao_evapotranspiration", "snowfall_sum", "weather_code",
            "wind_speed_10m_max",
            "relative_humidity_2m_max", "relative_humidity_2m_min"
          ), collapse = ","),
          hourly     = paste(c(
            "soil_moisture_0_to_7cm",
            "soil_moisture_7_to_28cm",
            "soil_temperature_0_to_7cm",
            "snow_depth"
          ), collapse = ",")
        ) |>
        .maybe_key() |>
        httr2::req_retry(
          max_tries    = 5,
          is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
          backoff      = \(i) 30 * 2^(i - 1L)
        ) |>
        httr2::req_perform()

      hist_body  <- .om_parse(hist_resp)

      hist_daily <- .om_to_tibble(hist_body$daily) |>
        rename(
          precip        = precipitation_sum,
          temp_max      = temperature_2m_max,
          temp_min      = temperature_2m_min,
          et0           = et0_fao_evapotranspiration,
          snowfall      = snowfall_sum,
          weathercode   = weather_code,
          windspeed_max = wind_speed_10m_max,
          humidity_max  = relative_humidity_2m_max,
          humidity_min  = relative_humidity_2m_min
        )

      hist_soil <- .om_to_tibble(hist_body$hourly) |>
        group_by(date) |>
        summarise(
          soil_moisture_0_1  = mean(soil_moisture_0_to_7cm,    na.rm = TRUE),
          soil_moisture_1_3  = mean(soil_moisture_0_to_7cm,    na.rm = TRUE),
          soil_moisture_3_9  = mean(soil_moisture_7_to_28cm,   na.rm = TRUE),
          soil_temp_0cm      = mean(soil_temperature_0_to_7cm, na.rm = TRUE),
          snow_depth         = mean(snow_depth, na.rm = TRUE) * 100,
          .groups = "drop"
        )

      result <- hist_daily |>
        left_join(hist_soil, by = "date") |>
        mutate(source = "history")

      saveRDS(result, hist_file)
      result

    }, error = function(e) {
      # ── Stale-if-error: use existing cache even if past TTL ───────────────
      if (file.exists(hist_file)) {
        warning(sprintf(
          "Archive API failed (%s). Using stale cache (%.0f h old).",
          conditionMessage(e), hist_age
        ))
        readRDS(hist_file)
      } else {
        stop(e)
      }
    })
  }

  # ── 2. FORECAST + RECENT — api.open-meteo.com (cached 1 h) ───────────────
  fc_age <- .cache_age_h(fc_file)
  if (fc_age < fc_max_age_h) {
    message(sprintf("Using cached forecast data (%.0f min old).", fc_age * 60))
    fc <- readRDS(fc_file)
  } else {
    message("Fetching recent + ", forecast_days, "-day forecast from Open-Meteo...")

    fc_resp <- httr2::request("https://api.open-meteo.com/v1/forecast") |>
      httr2::req_url_query(
        latitude      = lat,
        longitude     = lon,
        timezone      = tz,
        past_days     = past_days_fc,
        forecast_days = forecast_days,
        daily         = paste(c(
          "precipitation_sum", "temperature_2m_max", "temperature_2m_min",
          "et0_fao_evapotranspiration", "snowfall_sum", "weather_code",
          "wind_speed_10m_max",
          "relative_humidity_2m_max", "relative_humidity_2m_min"
        ), collapse = ","),
        hourly        = paste(c(
          "soil_moisture_0_to_1cm",
          "soil_moisture_1_to_3cm",
          "soil_moisture_3_to_9cm",
          "soil_temperature_6cm",
          "snow_depth"
        ), collapse = ",")
      ) |>
      .maybe_key() |>
      httr2::req_retry(
        max_tries    = 5,
        is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
        backoff      = \(i) 30 * 2^(i - 1L)
      ) |>
      httr2::req_perform()

    fc_body <- .om_parse(fc_resp)

    fc_daily <- .om_to_tibble(fc_body$daily) |>
      rename(
        precip        = precipitation_sum,
        temp_max      = temperature_2m_max,
        temp_min      = temperature_2m_min,
        et0           = et0_fao_evapotranspiration,
        snowfall      = snowfall_sum,
        weathercode   = weather_code,
        windspeed_max = wind_speed_10m_max,
        humidity_max  = relative_humidity_2m_max,
        humidity_min  = relative_humidity_2m_min
      )

    fc_soil <- .om_to_tibble(fc_body$hourly) |>
      group_by(date) |>
      summarise(
        soil_moisture_0_1  = mean(soil_moisture_0_to_1cm, na.rm = TRUE),
        soil_moisture_1_3  = mean(soil_moisture_1_to_3cm, na.rm = TRUE),
        soil_moisture_3_9  = mean(soil_moisture_3_to_9cm, na.rm = TRUE),
        soil_temp_0cm      = mean(soil_temperature_6cm,   na.rm = TRUE),
        snow_depth         = mean(snow_depth, na.rm = TRUE) * 100,
        .groups = "drop"
      )

    fc <- fc_daily |>
      left_join(fc_soil, by = "date") |>
      mutate(source = if_else(date > today, "forecast", "history"))

    saveRDS(fc, fc_file)
  }

  # ── 3. Combine (forecast/recent takes precedence over old archive) ─────────
  weather <- bind_rows(hist, fc) |>
    arrange(date, desc(source == "history")) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date) |>
    mutate(
      water_year = if_else(month(date) >= 10, year(date), year(date) - 1L),
      dowy       = as.integer(date - make_date(
                     if_else(month(date) >= 10, year(date), year(date) - 1L), 10, 1
                   )) + 1L,
      snow_year  = if_else(month(date) >= 11, year(date), year(date) - 1L),
      doss       = as.integer(date - make_date(
                     if_else(month(date) >= 11, year(date), year(date) - 1L), 11, 1
                   )) + 1L
    )

  weather
}
