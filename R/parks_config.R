# R/parks_config.R
# Load park configuration from data/parks.yml.
# In each park page, set park_id before sourcing this file:
#   park_id <- "hunters-creek"
#   source("../../R/parks_config.R")   # adjust path as needed

if (!exists("park_id")) stop("park_id must be defined before sourcing parks_config.R")

# Null-coalescing operator if not already defined (rlang may already provide it)
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
SNOW_START_MONTH <- park$snow_season$start_month %||% 9
SNOW_START_DAY   <- park$snow_season$start_day   %||% 1
SNOW_END_MONTH   <- park$snow_season$end_month   %||% 5
SNOW_END_DAY     <- park$snow_season$end_day     %||% 31
NORDIC_SKI_THRESHOLD_CM <- park$nordic_ski_threshold_cm %||% 10
# UI default tab window (Dec–Mar → Snow unless overridden by depth). Distinct from snow_season.
DEFAULT_SNOW_TAB_START_MONTH <- park$default_snow_tab$start_month %||% 12
DEFAULT_SNOW_TAB_START_DAY   <- park$default_snow_tab$start_day   %||% 1
DEFAULT_SNOW_TAB_END_MONTH   <- park$default_snow_tab$end_month   %||% 3
DEFAULT_SNOW_TAB_END_DAY     <- park$default_snow_tab$end_day     %||% 31
PARK_BOUNDARY    <- park$boundary_file %||% NULL
PARK_ID          <- park$id

# Human-readable winter window, e.g. "Sep–May"
SNOW_SEASON_LABEL <- paste0(
  month.abb[SNOW_START_MONTH], "\u2013", month.abb[SNOW_END_MONTH]
)

message("Loaded park: ", PARK_NAME)
