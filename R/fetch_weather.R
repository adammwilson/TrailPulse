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
    lat           = PARK_LAT,
    lon           = PARK_LON,
    start         = WEATHER_START,
    tz            = PARK_TZ,
    forecast_days = FORECAST_DAYS
) {
  today <- Sys.Date()

  # ERA5-Land has a ~5-day delay; use archive up to 10 days ago to be safe,
  # then cover the recent gap + forecast with the forecast API (past_days).
  hist_end      <- today - 10L
  past_days_fc  <- as.integer(today - hist_end) + 1L   # overlap by 1 day

  # ── 1. HISTORICAL — archive-api.open-meteo.com ──────────────────────────
  message("Fetching historical weather (", start, " to ", hist_end, ")...")

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
        "soil_moisture_0_to_7cm",   # ERA5-Land layer 1 (0-7 cm)
        "soil_moisture_7_to_28cm",  # ERA5-Land layer 2 (7-28 cm)
        "soil_temperature_0_to_7cm",
        "snow_depth"                # metres
      ), collapse = ",")
    ) |>
    httr2::req_retry(
      max_tries    = 5,
      is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
      backoff      = \(i) 10 * 2^(i - 1L)   # 10, 20, 40, 80 s
    ) |>
    httr2::req_perform()

  hist_body <- .om_parse(hist_resp)

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

  # Aggregate hourly soil to daily mean
  hist_soil <- .om_to_tibble(hist_body$hourly) |>
    group_by(date) |>
    summarise(
      # Use ERA5 layer 1 (0-7 cm) as proxy for all three fine slots
      soil_moisture_0_1  = mean(soil_moisture_0_to_7cm,    na.rm = TRUE),
      soil_moisture_1_3  = mean(soil_moisture_0_to_7cm,    na.rm = TRUE),
      soil_moisture_3_9  = mean(soil_moisture_7_to_28cm,   na.rm = TRUE),
      soil_temp_0cm      = mean(soil_temperature_0_to_7cm, na.rm = TRUE),
      snow_depth         = mean(snow_depth, na.rm = TRUE) * 100, # m → cm
      .groups = "drop"
    )

  hist <- hist_daily |>
    left_join(hist_soil, by = "date") |>
    mutate(source = "history")

  # ── 2. FORECAST + RECENT — api.open-meteo.com ───────────────────────────
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
        "soil_temperature_0cm",
        "snow_depth"    # metres
      ), collapse = ",")
    ) |>
    httr2::req_retry(
      max_tries    = 5,
      is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
      backoff      = \(i) 10 * 2^(i - 1L)
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
      soil_temp_0cm      = mean(soil_temperature_0cm,   na.rm = TRUE),
      snow_depth         = mean(snow_depth, na.rm = TRUE) * 100, # m → cm
      .groups = "drop"
    )

  fc <- fc_daily |>
    left_join(fc_soil, by = "date") |>
    mutate(source = if_else(date > today, "forecast", "history"))

  # ── 3. Combine (forecast/recent takes precedence over old archive) ───────
  weather <- bind_rows(hist, fc) |>
    # For overlapping dates keep the forecast-API row (more recent model)
    arrange(date, desc(source == "history")) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date) |>
    mutate(
      # Hydrologic water year: Oct 1 start
      water_year = if_else(month(date) >= 10, year(date), year(date) - 1L),
      dowy       = as.integer(date - make_date(
                     if_else(month(date) >= 10, year(date), year(date) - 1L), 10, 1
                   )) + 1L,
      # Snow season year: Nov 1 start
      snow_year  = if_else(month(date) >= 11, year(date), year(date) - 1L),
      doss       = as.integer(date - make_date(
                     if_else(month(date) >= 11, year(date), year(date) - 1L), 11, 1
                   )) + 1L
    )

  weather
}
