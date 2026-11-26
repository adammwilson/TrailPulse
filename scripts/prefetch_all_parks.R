#!/usr/bin/env Rscript
# scripts/prefetch_all_parks.R
# Run from the project root before quarto render to warm the weather cache for
# every park defined in data/parks.yml.
#
# Usage: Rscript scripts/prefetch_all_parks.R
 
suppressPackageStartupMessages({
  library(yaml)
  library(dplyr)
  library(lubridate)
  library(httr2)
  library(tibble)
})

source("R/setup.R")
source("R/fetch_weather.R")
source("R/fetch_streamflow.R")

all_parks <- yaml::read_yaml("data/parks.yml")[["parks"]]

message("\n=== Pre-fetching weather & streamflow data for ", length(all_parks), " parks ===\n")

for (park in all_parks) {
  message("--- ", park$name, " (", park$id, ") ---")

  cache_dir <- file.path("parks", park$id, "data", "cache")
  snow_start_month <- if (!is.null(park$snow_season$start_month)) park$snow_season$start_month else 9L
  snow_start_day   <- if (!is.null(park$snow_season$start_day))   park$snow_season$start_day   else 1L

  tryCatch({
    result <- fetch_weather(
      lat              = park$lat,
      lon              = park$lon,
      start            = as.Date(park$weather_history_start),
      tz               = park$timezone,
      cache_dir        = cache_dir,
      snow_start_month = snow_start_month,
      snow_start_day   = snow_start_day
    )
    if (is.null(result)) {
      warning("fetch_weather() returned NULL for ", park$name)
    } else {
      message("OK — ", nrow(result), " days of weather cached in ", cache_dir)
    }
  }, error = function(e) {
    message("ERROR fetching weather for ", park$name, ": ", e$message)
  })

  tryCatch({
    sf_daily <- fetch_streamflow(
      gauge     = park$usgs_gauge,
      start     = as.Date(park$weather_history_start),
      tz        = park$timezone,
      cache_dir = cache_dir
    )
    message("OK — ", nrow(sf_daily), " days of daily streamflow cached in ", cache_dir)
  }, error = function(e) {
    message("ERROR fetching daily streamflow for ", park$name, ": ", e$message)
  })

  tryCatch({
    sf_uv <- fetch_streamflow_uv(
      gauge     = park$usgs_gauge,
      tz        = park$timezone,
      cache_dir = cache_dir
    )
    message("OK — ", nrow(sf_uv), " rows of 15-min streamflow cached in ", cache_dir)
  }, error = function(e) {
    message("ERROR fetching 15-min streamflow for ", park$name, ": ", e$message)
  })

  # Pause between parks to avoid Open-Meteo rate limits
  Sys.sleep(3)
}

message("\n=== Pre-fetch complete ===\n")
