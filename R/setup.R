# R/setup.R
# Load all packages and set global options/themes.
# Source this at the top of every park page.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(httr2)
  library(dataRetrieval)
  library(httr2)
  library(dygraphs)
  library(xts)
  library(zoo)
  library(yaml)
  library(htmltools)
  library(scales)
  library(glue)
})

# ── Global ggplot theme ─────────────────────────────────────────────────────
theme_trailpulse <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size * 1.2),
      plot.subtitle    = element_text(color = "grey40", size = base_size * 0.95),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(size = base_size * 0.9, color = "grey30"),
      legend.position  = "bottom"
    )
}

theme_set(theme_trailpulse())

# ── Mud level palette ────────────────────────────────────────────────────────
MUD_LEVELS <- c(
  "Bone Dry"   = 0,
  "Dusty"      = 1.5,
  "Firm"       = 3,
  "Tacky"      = 4.5,
  "Soft"       = 6,
  "Muddy"      = 7,
  "Very Muddy" = 8.5,
  "Impassable" = 10
)

MUD_COLORS <- c(
  "Bone Dry"   = "#d4a96a",
  "Dusty"      = "#c9a060",
  "Firm"       = "#b8864a",
  "Tacky"      = "#a06c34",
  "Soft"       = "#8b5a2b",
  "Muddy"      = "#7a4a22",
  "Very Muddy" = "#6b3b18",
  "Impassable" = "#5a2e10"
)

# ── Snow level palette ───────────────────────────────────────────────────────
SNOW_LEVELS <- c(
  "Bare"       = 0,
  "Dusting"    = 2,
  "Skiable"    = 8,
  "Good"       = 20,
  "Powder Day" = 35
)

SNOW_COLORS <- c(
  "Bare"       = "#b0bec5",
  "Dusting"    = "#90a4ae",
  "Skiable"    = "#64b5f6",
  "Good"       = "#1e88e5",
  "Powder Day" = "#0d47a1"
)

# ── Helper: mud level name from numeric score ────────────────────────────────
mud_level_name <- function(score) {
  if (is.na(score)) return(NA_character_)
  breaks <- c(-Inf, 1, 2.5, 4, 5.5, 6.75, 7.75, 9, Inf)
  labels <- names(MUD_LEVELS)
  labels[findInterval(score, breaks)]
}

mud_level_color <- function(score) {
  MUD_COLORS[mud_level_name(score)]
}

# ── Helper: snow level name from depth (cm) ──────────────────────────────────
snow_level_name <- function(depth_cm) {
  if (is.na(depth_cm)) return("Bare")
  case_when(
    depth_cm <= 0   ~ "Bare",
    depth_cm < 5    ~ "Dusting",
    depth_cm < 15   ~ "Skiable",
    depth_cm < 30   ~ "Good",
    TRUE            ~ "Powder Day"
  )
}

snow_level_color <- function(depth_cm) {
  SNOW_COLORS[snow_level_name(depth_cm)]
}

# ── Forecast window ──────────────────────────────────────────────────────────
HISTORY_DAYS  <- 14L   # days of observed data to show in current-conditions charts
FORECAST_DAYS <- 7L    # days of forecast to show
