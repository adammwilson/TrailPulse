# R/parks_config.R
# Load park configuration from data/parks.yml.
# In each park page, set park_id before sourcing this file:
#   park_id <- "hunters-creek"
#   source("../../R/parks_config.R")   # adjust path as needed

if (!exists("park_id")) stop("park_id must be defined before sourcing parks_config.R")

# Resolve path relative to project root (works whether called from project root
# or from a subdirectory like parks/hunters-creek/).
.config_path <- if (file.exists("data/parks.yml")) {
  "data/parks.yml"
} else if (file.exists("../../data/parks.yml")) {
  "../../data/parks.yml"
} else {
  stop("Cannot locate data/parks.yml")
}

.all_parks <- yaml::read_yaml(.config_path)[["parks"]]

# Find this park by id
park <- Filter(function(p) p$id == park_id, .all_parks)
if (length(park) == 0) stop("Park '", park_id, "' not found in data/parks.yml")
park <- park[[1]]

# Convenience scalars
PARK_NAME        <- park$name
PARK_LAT         <- park$lat
PARK_LON         <- park$lon
PARK_TZ          <- park$timezone
PARK_USGS        <- park$usgs_gauge
WEATHER_START    <- as.Date(park$weather_history_start)
MUD_MIDPOINT     <- park$mud_calibration$midpoint              %||% 0.35
MUD_STEEPNESS    <- park$mud_calibration$steepness             %||% 12
MUD_PRECIP_BOOST <- park$mud_calibration$precip_boost          %||% 1.0
MUD_SOIL_SCALE   <- park$mud_calibration$soil_moisture_scale   %||% 1.0
HIST_SOIL_SCALE  <- park$mud_calibration$hist_soil_scale       %||% 1.0
SNOW_START_MONTH <- park$snow_season$start_month %||% 11
SNOW_END_MONTH   <- park$snow_season$end_month   %||% 4
PARK_BOUNDARY    <- park$boundary_file %||% NULL
PARK_ID          <- park$id

# Null-coalescing operator if not already defined
`%||%` <- function(a, b) if (!is.null(a)) a else b

message("Loaded park: ", PARK_NAME)
