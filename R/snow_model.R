# R/snow_model.R
# Snow season analysis: cumulative snowfall, comparisons to historical years.
# Depends on: setup.R, parks_config.R, condition.R
# ── Add unified snow condition columns to a weather tibble ──────────────────
# Appends snow_condition_score and snow_condition_label to every row.
# Call on the output of compute_mud_level() before passing to plots/strips.
compute_snow_condition <- function(df) {
  df |>
    mutate(
      snow_condition_score = snow_to_condition_score(coalesce(snow_depth, 0)),
      snow_condition_label = score_to_label(snow_condition_score)
    )
}
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

  cond_score <- snow_to_condition_score(depth_today)

  list(
    snow_year        = current_year,
    depth_today_cm   = depth_today,
    cum_snowfall_cm  = cum_so_far,
    snow_pct_rank    = snow_pct_rank,
    condition_score  = cond_score,
    condition_label  = score_to_label(cond_score),
    current_season   = current_season,
    peak_df          = peak_df
  )
}

# snow_oneliner() removed — use condition_oneliner(mode="snow", ...) from condition.R
