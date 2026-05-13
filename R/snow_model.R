# R/snow_model.R
# Snow season analysis: cumulative snowfall, comparisons to historical years.
# Depends on: setup.R, parks_config.R

# ── Snow season filter ───────────────────────────────────────────────────────
# A "snow season" runs from Nov 1 of year Y to Apr 30 of year Y+1,
# labelled by Y (e.g. snow_year 2024 = Nov 2024 – Apr 2025).

is_snow_season <- function(date) {
  m <- month(date)
  m >= SNOW_START_MONTH | m <= SNOW_END_MONTH
}

# ── Compute per-season cumulative snowfall tibble ────────────────────────────
# Returns a tibble suitable for the spaghetti dygraph:
#   snow_year, doss (day of snow season), cum_snowfall_cm
compute_cumulative_snowfall <- function(weather_df) {
  weather_df |>
    filter(is_snow_season(date)) |>
    filter(!is.na(snowfall)) |>
    group_by(snow_year) |>
    arrange(date) |>
    mutate(cum_snowfall_cm = cumsum(coalesce(snowfall, 0))) |>
    ungroup() |>
    select(snow_year, doss, date, cum_snowfall_cm)
}

# ── Peak snow depth per season ───────────────────────────────────────────────
compute_peak_snow_depth <- function(weather_df) {
  weather_df |>
    filter(is_snow_season(date), !is.na(snow_depth)) |>
    group_by(snow_year) |>
    summarise(
      peak_depth_cm   = max(snow_depth, na.rm = TRUE),
      peak_date       = date[which.max(snow_depth)],
      total_snow_days = sum(snow_depth > 2, na.rm = TRUE),
      cum_snowfall_cm = sum(coalesce(snowfall, 0), na.rm = TRUE),
      .groups = "drop"
    )
}

# ── Current season summary ───────────────────────────────────────────────────
current_snow_summary <- function(weather_df) {
  current_year <- if (month(Sys.Date()) >= SNOW_START_MONTH) {
    year(Sys.Date())
  } else {
    year(Sys.Date()) - 1L
  }

  today_row <- weather_df |>
    filter(date <= Sys.Date()) |>
    slice_tail(n = 1)

  current_season <- weather_df |>
    filter(snow_year == current_year, is_snow_season(date), !is.na(snowfall)) |>
    arrange(date) |>
    mutate(cum_snowfall_cm = cumsum(coalesce(snowfall, 0)))

  peak_df <- compute_peak_snow_depth(weather_df)
  current_peak <- peak_df |> filter(snow_year == current_year)
  hist_peak    <- peak_df |> filter(snow_year < current_year)

  cum_so_far  <- if (nrow(current_season) > 0) max(current_season$cum_snowfall_cm) else 0
  depth_today <- coalesce(today_row$snow_depth[[1]], 0)

  # Percentile rank among completed historical seasons (same day of season)
  current_doss <- if (nrow(current_season) > 0) max(current_season$doss) else 0

  hist_at_same_doss <- compute_cumulative_snowfall(weather_df) |>
    filter(snow_year < current_year, doss <= current_doss) |>
    group_by(snow_year) |>
    slice_max(doss, n = 1) |>
    ungroup()

  snow_pct_rank <- if (nrow(hist_at_same_doss) > 0) {
    round(100 * mean(hist_at_same_doss$cum_snowfall_cm < cum_so_far))
  } else {
    NA_real_
  }

  list(
    snow_year        = current_year,
    depth_today_cm   = depth_today,
    cum_snowfall_cm  = cum_so_far,
    snow_pct_rank    = snow_pct_rank,
    current_season   = current_season,
    peak_df          = peak_df
  )
}

# ── Snow one-liner ────────────────────────────────────────────────────────────
snow_oneliner <- function(depth_cm, pct_rank = NULL) {
  lvl <- snow_level_name(depth_cm)

  base <- switch(lvl,
    "Bare"       = "No snow on the ground currently.",
    "Dusting"    = glue("Just a light dusting ({round(depth_cm)} cm) \u2014 not yet skiable."),
    "Skiable"    = glue("{round(depth_cm)} cm on the ground \u2014 skiable on groomed trails."),
    "Good"       = glue("{round(depth_cm)} cm of snow \u2014 good Nordic ski conditions."),
    "Powder Day" = glue("{round(depth_cm)} cm of snow \u2014 powder day! Get out there!"),
    "No snow data."
  )

  if (!is.null(pct_rank) && !is.na(pct_rank) && depth_cm > 0) {
    base <- glue("{base} More snowfall than {pct_rank}% of seasons on record.")
  }

  base
}
