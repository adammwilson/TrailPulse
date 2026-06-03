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

all_parks <- yaml::read_yaml("data/parks.yml")[["parks"]]

message("\n=== Pre-fetching weather data for ", length(all_parks), " parks ===\n")

for (park in all_parks) {
  message("--- ", park$name, " (", park$id, ") ---")

  cache_dir <- file.path("parks", park$id, "data", "cache")

  tryCatch({
    result <- fetch_weather(
      lat       = park$lat,
      lon       = park$lon,
      start     = as.Date(park$weather_history_start),
      tz        = park$timezone,
      cache_dir = cache_dir
    )
    if (is.null(result)) {
      warning("fetch_weather() returned NULL for ", park$name)
    } else {
      message("OK — ", nrow(result), " days of data cached in ", cache_dir)
    }
  }, error = function(e) {
    message("ERROR fetching weather for ", park$name, ": ", e$message)
  })

  # Pause between parks to avoid Open-Meteo rate limits
  Sys.sleep(3)
}

message("\n=== Pre-fetch complete ===\n")
