# R/fetch_streamflow.R
# Fetch USGS streamflow for a park's gauge.
# Two functions:
#   fetch_streamflow()    — daily mean values (for historical spaghetti plots)
#   fetch_streamflow_uv() — 15-min unit values (for the current-conditions panel)
#
# Depends on: setup.R, parks_config.R

fetch_streamflow <- function(
    gauge = PARK_USGS,
    start = WEATHER_START,
    tz    = PARK_TZ
) {
  if (is.null(gauge) || is.na(gauge) || !nzchar(as.character(gauge))) {
    message("No USGS gauge configured for this park — skipping streamflow.")
    return(tibble(
      date = as.Date(character()),
      discharge_cfs = numeric(),
      discharge_cms = numeric(),
      roll7_cfs = numeric(),
      water_year = integer(),
      dowy = integer()
    ))
  }

  message("Fetching USGS streamflow for gauge ", gauge, "...")

  raw <- dataRetrieval::readNWISdv(
    siteNumbers  = gauge,
    parameterCd  = "00060",        # discharge, cubic feet/second
    startDate    = as.character(start),
    endDate      = as.character(Sys.Date())
  )

  if (nrow(raw) == 0) {
    warning("No streamflow data returned for gauge ", gauge)
    return(tibble(
      date = as.Date(character()),
      discharge_cfs = numeric(),
      discharge_cms = numeric(),
      roll7_cfs = numeric(),
      water_year = integer(),
      dowy = integer()
    ))
  }

  raw |>
    dataRetrieval::renameNWISColumns() |>
    as_tibble() |>
    rename(
      date          = Date,
      discharge_cfs = Flow
    ) |>
    mutate(
      discharge_cms = discharge_cfs * 0.0283168,
      roll7_cfs     = zoo::rollmedian(discharge_cfs, k = 7, fill = NA, align = "right"),
      water_year    = if_else(month(date) >= 10, year(date), year(date) - 1L),
      dowy          = as.integer(date - make_date(
        if_else(month(date) >= 10, year(date), year(date) - 1L), 10, 1
      )) + 1L
    ) |>
    select(date, discharge_cfs, discharge_cms, roll7_cfs, water_year, dowy) |>
    arrange(date)
}

# ── 15-minute unit-value fetch ────────────────────────────────────────────────
# Returns datetime (POSIXct, local tz) + discharge_cfs for the last `days` days.
# Used for the current-conditions streamflow panel; NOT suitable for historical
# spaghetti plots (use fetch_streamflow() for those).

fetch_streamflow_uv <- function(
    gauge = PARK_USGS,
    days  = HISTORY_DAYS + FORECAST_DAYS + 2L,
    tz    = PARK_TZ
) {
  empty <- tibble(datetime = as.POSIXct(character()), discharge_cfs = numeric())

  if (is.null(gauge) || is.na(gauge) || !nzchar(as.character(gauge))) {
    message("No USGS gauge configured — skipping 15-min streamflow.")
    return(empty)
  }

  message("Fetching USGS 15-min streamflow for gauge ", gauge, "...")

  raw <- tryCatch(
    dataRetrieval::readNWISuv(
      siteNumbers = gauge,
      parameterCd = "00060",
      startDate   = as.character(Sys.Date() - days),
      endDate     = as.character(Sys.Date())
    ),
    error = function(e) {
      warning("UV streamflow fetch failed for gauge ", gauge, ": ", e$message)
      NULL
    }
  )

  if (is.null(raw) || nrow(raw) == 0) {
    warning("No 15-min streamflow data returned for gauge ", gauge)
    return(empty)
  }

  raw |>
    dataRetrieval::renameNWISColumns() |>
    as_tibble() |>
    rename(datetime = dateTime, discharge_cfs = Flow_Inst) |>
    mutate(datetime = lubridate::with_tz(datetime, tz)) |>
    filter(!is.na(discharge_cfs), discharge_cfs >= 0) |>
    select(datetime, discharge_cfs) |>
    arrange(datetime)
}
