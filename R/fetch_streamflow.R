# R/fetch_streamflow.R
# Fetch USGS daily streamflow for a park's gauge.
# Returns a tidy tibble with water_year and rolling median.
#
# Depends on: setup.R, parks_config.R

fetch_streamflow <- function(
    gauge = PARK_USGS,
    start = WEATHER_START,
    tz    = PARK_TZ
) {
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
