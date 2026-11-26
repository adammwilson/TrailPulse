# R/fetch_streamflow.R
# Fetch USGS streamflow for a park's gauge.
# Two functions:
#   fetch_streamflow()    — daily mean values (for historical spaghetti plots)
#   fetch_streamflow_uv() — 15-min unit values (for the current-conditions panel)
#
# Primary source: USGS Water Data OGC API (api.waterdata.usgs.gov/ogcapi/v0).
# Fallback source: legacy NWIS web services via dataRetrieval::readNWIS*().
# Both functions cache results to RDS in `cache_dir` and fall back to stale
# cache (or an empty tibble) on transient outages so site renders never abort.
#
# Depends on: setup.R, parks_config.R

.USGS_OGC_BASE <- "https://api.waterdata.usgs.gov/ogcapi/v0/collections"
.USGS_UA       <- "TrailPulse/1.0 (https://github.com/adammwilson/TrailPulse)"

.usgs_location_id <- function(gauge) {
  g <- trimws(as.character(gauge))
  if (grepl("^[A-Za-z]+-", g)) g else paste0("USGS-", g)
}

# Retry wrapper covering curl/TLS/timeout transport failures as well as errors
.usgs_with_retry <- function(fn, max_tries = 3L, base_delay = 3) {
  last_err <- NULL
  for (attempt in seq_len(max_tries)) {
    res <- tryCatch(
      list(ok = TRUE, value = fn()),
      error = function(e) list(ok = FALSE, error = e)
    )
    if (isTRUE(res$ok)) return(res$value)
    last_err <- res$error
    if (attempt < max_tries) {
      delay <- base_delay * 2^(attempt - 1L)
      message(sprintf(
        "USGS request attempt %d/%d failed (%s); retrying in %.0f s...",
        attempt, max_tries, conditionMessage(last_err), delay
      ))
      Sys.sleep(delay)
    }
  }
  stop(last_err)
}

# Fetch paginated CSV from the USGS Water Data OGC API (no sortby so offset works)
.usgs_ogc_fetch_csv <- function(
    collection,
    query,
    page_limit = 50000L,
    api_key    = Sys.getenv("API_USGS_PAT", unset = "")
) {
  url    <- paste0(.USGS_OGC_BASE, "/", collection, "/items")
  pages  <- list()
  offset <- 0L

  repeat {
    req <- httr2::request(url) |>
      httr2::req_headers("User-Agent" = .USGS_UA) |>
      httr2::req_url_query(
        !!!query,
        skipGeometry = "true",
        limit        = page_limit,
        offset       = offset,
        f            = "csv"
      ) |>
      httr2::req_timeout(30) |>
      httr2::req_options(connecttimeout = 20) |>
      httr2::req_retry(
        max_tries    = 3L,
        is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
        backoff      = \(i) 3 * 2^(i - 1L)
      )

    if (nzchar(api_key)) {
      req <- httr2::req_headers(req, "X-Api-Key" = api_key)
    }

    resp <- .usgs_with_retry(\() httr2::req_perform(req), max_tries = 3L, base_delay = 3)
    txt  <- httr2::resp_body_string(resp)
    if (!nzchar(trimws(txt))) break

    page <- readr::read_csv(
      I(txt),
      col_types      = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress       = FALSE
    )
    if (nrow(page) == 0L) break

    pages[[length(pages) + 1L]] <- page
    if (nrow(page) < page_limit) break
    offset <- offset + page_limit
  }

  if (length(pages) == 0L) return(tibble::tibble())
  dplyr::bind_rows(pages)
}

.usgs_empty_daily <- function() {
  tibble(
    date          = as.Date(character()),
    discharge_cfs = numeric(),
    discharge_cms = numeric(),
    roll7_cfs     = numeric(),
    water_year    = integer(),
    dowy          = integer()
  )
}

.usgs_derive_daily <- function(df) {
  cleaned <- df |>
    filter(!is.na(date), !is.na(discharge_cfs)) |>
    arrange(date) |>
    distinct(date, .keep_all = TRUE)

  if (nrow(cleaned) == 0L) return(.usgs_empty_daily())

  cleaned |>
    mutate(
      discharge_cms = discharge_cfs * 0.0283168,
      roll7_cfs     = if (n() >= 7L)
        zoo::rollmedian(discharge_cfs, k = 7, fill = NA, align = "right")
      else
        rep(NA_real_, n()),
      water_year    = as.integer(if_else(month(date) >= 10, year(date), year(date) - 1L)),
      dowy          = as.integer(date - make_date(
        water_year, 10, 1
      )) + 1L
    ) |>
    select(date, discharge_cfs, discharge_cms, roll7_cfs, water_year, dowy) |>
    arrange(date)
}

.usgs_fetch_daily_live <- function(gauge, fetch_start, fetch_end) {
  # Primary: modern USGS Water Data OGC API
  ogc <- tryCatch({
    raw <- .usgs_ogc_fetch_csv(
      collection = "daily",
      query = list(
        monitoring_location_id = .usgs_location_id(gauge),
        parameter_code         = "00060",
        statistic_id           = "00003",
        time                   = paste0(format(fetch_start), "/", format(fetch_end)),
        properties             = "time,value"
      )
    )
    if (nrow(raw) == 0L) {
      tibble(date = as.Date(character()), discharge_cfs = numeric())
    } else {
      tibble(
        date          = as.Date(raw$time),
        discharge_cfs = suppressWarnings(as.numeric(raw$value))
      ) |>
        filter(!is.na(date), !is.na(discharge_cfs))
    }
  }, error = function(e) {
    warning("USGS OGC daily API failed for gauge ", gauge, ": ", conditionMessage(e))
    NULL
  })

  if (!is.null(ogc)) return(ogc)

  # Fallback: legacy NWIS daily-values service via dataRetrieval
  message("Falling back to legacy NWIS daily service for gauge ", gauge, "...")
  raw <- .usgs_with_retry(
    \() dataRetrieval::readNWISdv(
      siteNumbers = gauge,
      parameterCd = "00060",
      startDate   = as.character(fetch_start),
      endDate     = as.character(fetch_end)
    ),
    max_tries  = 3L,
    base_delay = 3
  )

  if (is.null(raw) || nrow(raw) == 0L) {
    return(tibble(date = as.Date(character()), discharge_cfs = numeric()))
  }

  raw |>
    dataRetrieval::renameNWISColumns() |>
    as_tibble() |>
    transmute(
      date          = as.Date(Date),
      discharge_cfs = suppressWarnings(as.numeric(Flow))
    ) |>
    filter(!is.na(date), !is.na(discharge_cfs))
}

fetch_streamflow <- function(
    gauge     = PARK_USGS,
    start     = WEATHER_START,
    tz        = PARK_TZ,
    cache_dir = "data/cache",
    max_age_h = 6
) {
  if (is.null(gauge) || is.na(gauge) || !nzchar(as.character(gauge))) {
    message("No USGS gauge configured for this park — skipping streamflow.")
    return(.usgs_empty_daily())
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- file.path(cache_dir, paste0("streamflow_dv_", gauge, ".rds"))
  cache_age_h <- if (file.exists(cache_file)) {
    as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "hours"))
  } else {
    Inf
  }

  cached <- if (file.exists(cache_file)) {
    tryCatch(readRDS(cache_file), error = function(e) NULL)
  } else {
    NULL
  }

  if (!is.null(cached) && nrow(cached) > 0L && cache_age_h < max_age_h) {
    message(sprintf("Using cached USGS daily streamflow for %s (%.1f h old).",
                    gauge, cache_age_h))
    return(cached)
  }

  start_date <- as.Date(start)
  today      <- Sys.Date()

  # Incremental fetch when cache already covers from `start` (overlap 14 days
  # to pick up provisional revisions)
  fetch_start <- if (!is.null(cached) && nrow(cached) > 0L &&
                     min(cached$date) <= start_date + 7L) {
    max(start_date, max(cached$date) - 14L)
  } else {
    start_date
  }

  message("Fetching USGS streamflow for gauge ", gauge,
          " (", fetch_start, " to ", today, ")...")

  new_rows <- tryCatch(
    .usgs_fetch_daily_live(gauge, fetch_start, today),
    error = function(e) {
      warning("All USGS daily streamflow endpoints failed for gauge ", gauge,
              ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(new_rows)) {
    stale_max_h <- 720
    if (!is.null(cached) && nrow(cached) > 0L && cache_age_h <= stale_max_h) {
      warning(sprintf(
        "Using stale USGS daily streamflow cache for %s (%.0f h old).",
        gauge, cache_age_h
      ))
      return(cached)
    }
    return(.usgs_empty_daily())
  }

  if (nrow(new_rows) == 0L) {
    if (!is.null(cached) && nrow(cached) > 0L) {
      saveRDS(cached, cache_file)
      return(cached)
    }
    warning("No streamflow data returned for gauge ", gauge)
    return(.usgs_empty_daily())
  }

  combined <- if (!is.null(cached) && nrow(cached) > 0L) {
    bind_rows(
      cached |> filter(date < min(new_rows$date)) |> select(date, discharge_cfs),
      new_rows
    )
  } else {
    new_rows
  }

  result <- .usgs_derive_daily(combined)
  if (nrow(result) > 0L) saveRDS(result, cache_file)
  result
}

# ── 15-minute unit-value fetch ────────────────────────────────────────────────
# Returns datetime (POSIXct, local tz) + discharge_cfs for the last `days` days.
# Used for the current-conditions streamflow panel; NOT suitable for historical
# spaghetti plots (use fetch_streamflow() for those).

.usgs_fetch_uv_live <- function(gauge, days, tz) {
  start_date <- Sys.Date() - days
  # Add 1 day to UTC end bound so evening local observations are included
  end_utc    <- Sys.Date() + 1L

  ogc <- tryCatch({
    raw <- .usgs_ogc_fetch_csv(
      collection = "continuous",
      query = list(
        monitoring_location_id = .usgs_location_id(gauge),
        parameter_code         = "00060",
        time                   = paste0(format(start_date), "T00:00:00Z/",
                                        format(end_utc), "T23:59:59Z"),
        properties             = "time,value"
      )
    )
    if (nrow(raw) == 0L) {
      tibble(datetime = as.POSIXct(character()), discharge_cfs = numeric())
    } else {
      tibble(
        datetime      = lubridate::with_tz(
          lubridate::ymd_hms(raw$time, tz = "UTC", quiet = TRUE),
          tz
        ),
        discharge_cfs = suppressWarnings(as.numeric(raw$value))
      ) |>
        filter(!is.na(datetime), !is.na(discharge_cfs), discharge_cfs >= 0) |>
        arrange(datetime) |>
        distinct(datetime, .keep_all = TRUE)
    }
  }, error = function(e) {
    warning("USGS OGC continuous API failed for gauge ", gauge, ": ", conditionMessage(e))
    NULL
  })

  if (!is.null(ogc)) return(ogc)

  message("Falling back to legacy NWIS UV service for gauge ", gauge, "...")
  raw <- .usgs_with_retry(
    \() dataRetrieval::readNWISuv(
      siteNumbers = gauge,
      parameterCd = "00060",
      startDate   = as.character(start_date),
      endDate     = as.character(Sys.Date())
    ),
    max_tries  = 3L,
    base_delay = 3
  )

  if (is.null(raw) || nrow(raw) == 0L) {
    return(tibble(datetime = as.POSIXct(character()), discharge_cfs = numeric()))
  }

  raw |>
    dataRetrieval::renameNWISColumns() |>
    as_tibble() |>
    rename(datetime = dateTime, discharge_cfs = Flow_Inst) |>
    mutate(datetime = lubridate::with_tz(datetime, tz)) |>
    filter(!is.na(datetime), !is.na(discharge_cfs), discharge_cfs >= 0) |>
    select(datetime, discharge_cfs) |>
    arrange(datetime) |>
    distinct(datetime, .keep_all = TRUE)
}

fetch_streamflow_uv <- function(
    gauge     = PARK_USGS,
    days      = HISTORY_DAYS + FORECAST_DAYS + 2L,
    tz        = PARK_TZ,
    cache_dir = "data/cache",
    max_age_h = 1
) {
  empty <- tibble(datetime = as.POSIXct(character()), discharge_cfs = numeric())

  if (is.null(gauge) || is.na(gauge) || !nzchar(as.character(gauge))) {
    message("No USGS gauge configured — skipping 15-min streamflow.")
    return(empty)
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- file.path(cache_dir, paste0("streamflow_uv_", gauge, ".rds"))
  cache_age_h <- if (file.exists(cache_file)) {
    as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "hours"))
  } else {
    Inf
  }

  cached <- if (file.exists(cache_file)) {
    tryCatch(readRDS(cache_file), error = function(e) NULL)
  } else {
    NULL
  }

  if (!is.null(cached) && nrow(cached) > 0L && cache_age_h < max_age_h) {
    message(sprintf("Using cached USGS 15-min streamflow for %s (%.0f min old).",
                    gauge, cache_age_h * 60))
    return(cached)
  }

  message("Fetching USGS 15-min streamflow for gauge ", gauge, "...")

  result <- tryCatch(
    .usgs_fetch_uv_live(gauge, days, tz),
    error = function(e) {
      warning("UV streamflow fetch failed for gauge ", gauge, ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result) || nrow(result) == 0L) {
    stale_max_h <- 720
    if (!is.null(cached) && nrow(cached) > 0L && cache_age_h <= stale_max_h) {
      warning(sprintf(
        "Using stale USGS 15-min streamflow cache for %s (%.0f h old).",
        gauge, cache_age_h
      ))
      return(cached)
    }
    warning("No 15-min streamflow data returned for gauge ", gauge)
    return(empty)
  }

  saveRDS(result, cache_file)
  result
}
