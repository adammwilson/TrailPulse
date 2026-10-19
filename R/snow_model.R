# R/snow_model.R
# Snow season analysis: cumulative snowfall, Nordic ski index, historical comparisons.
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

# ── Season date helpers (driven by parks.yml SNOW_* constants) ───────────────
# Winter season W = start of year W → end in year W+1 when the window crosses
# the calendar year (e.g. Sep 1 W → May 31 W+1). Labelled by start year W.

assign_snow_year <- function(date) {
  m <- month(date)
  d <- day(date)
  as.integer(dplyr::if_else(
    m > SNOW_START_MONTH | (m == SNOW_START_MONTH & d >= SNOW_START_DAY),
    year(date),
    year(date) - 1L
  ))
}

day_of_snow_season <- function(date) {
  sy <- assign_snow_year(date)
  season_start <- make_date(sy, SNOW_START_MONTH, SNOW_START_DAY)
  as.integer(date - season_start) + 1L
}

is_snow_season <- function(date) {
  m <- month(date)
  d <- day(date)
  after_start <- m > SNOW_START_MONTH | (m == SNOW_START_MONTH & d >= SNOW_START_DAY)
  before_end  <- m < SNOW_END_MONTH   | (m == SNOW_END_MONTH   & d <= SNOW_END_DAY)

  if (SNOW_START_MONTH > SNOW_END_MONTH) {
    # Crosses calendar year (e.g. Sep–May, Nov–Apr)
    after_start | before_end
  } else {
    after_start & before_end
  }
}

current_snow_year <- function(date = Sys.Date()) {
  assign_snow_year(date)[[1]]
}

# Attach snow_year / doss columns (used by fetch_weather and any re-labeling)
annotate_snow_season <- function(df) {
  df |>
    mutate(
      snow_year = assign_snow_year(date),
      doss      = day_of_snow_season(date)
    )
}

# ── Compute per-season cumulative snowfall tibble ────────────────────────────
# Returns a tibble suitable for the spaghetti chart:
#   snow_year, doss (day of snow season), cum_snowfall_cm
compute_cumulative_snowfall <- function(weather_df) {
  weather_df |>
    filter(is_snow_season(date)) |>
    filter(!is.na(snowfall)) |>
    filter(is.na(source) | source != "forecast") |>
    group_by(snow_year) |>
    arrange(date) |>
    mutate(cum_snowfall_cm = cumsum(coalesce(snowfall, 0))) |>
    ungroup() |>
    select(snow_year, doss, date, cum_snowfall_cm)
}

# ── Peak snow depth per season ───────────────────────────────────────────────
compute_peak_snow_depth <- function(weather_df,
                                    threshold_cm = NORDIC_SKI_THRESHOLD_CM) {
  weather_df |>
    filter(is_snow_season(date), !is.na(snow_depth)) |>
    filter(is.na(source) | source != "forecast") |>
    group_by(snow_year) |>
    summarise(
      peak_depth_cm   = max(snow_depth, na.rm = TRUE),
      peak_date       = date[which.max(snow_depth)],
      total_snow_days = sum(snow_depth >= threshold_cm, na.rm = TRUE),
      cum_snowfall_cm = sum(coalesce(snowfall, 0), na.rm = TRUE),
      .groups = "drop"
    )
}

# ── Rolling 7-day median snow depth (for spaghetti vs DOSS) ──────────────────
# Returns snow_year, doss, date, median_7d_depth_cm
compute_median_7day_snow_depth <- function(weather_df) {
  weather_df |>
    filter(is_snow_season(date)) |>
    filter(is.na(source) | source != "forecast") |>
    filter(!is.na(snow_depth)) |>
    arrange(date) |>
    group_by(snow_year) |>
    mutate(
      median_7d_depth_cm = zoo::rollapply(
        snow_depth,
        width = 7L,
        FUN = median,
        na.rm = TRUE,
        partial = TRUE,
        align = "right"
      )
    ) |>
    ungroup() |>
    select(snow_year, doss, date, median_7d_depth_cm)
}

# ── Rolling 7-day median daily snowfall (for spaghetti vs DOSS) ──────────────
# Returns snow_year, doss, date, median_7d_snowfall_cm
compute_median_7day_snowfall <- function(weather_df) {
  weather_df |>
    filter(is_snow_season(date)) |>
    filter(is.na(source) | source != "forecast") |>
    filter(!is.na(snowfall)) |>
    arrange(date) |>
    group_by(snow_year) |>
    mutate(
      median_7d_snowfall_cm = zoo::rollapply(
        snowfall,
        width = 7L,
        FUN = median,
        na.rm = TRUE,
        partial = TRUE,
        align = "right"
      )
    ) |>
    ungroup() |>
    select(snow_year, doss, date, median_7d_snowfall_cm)
}

# ── Cumulative ski days by DOSS (depth >= park threshold) ────────────────────
# Returns snow_year, doss, date, cum_ski_days
compute_cumulative_ski_days <- function(weather_df,
                                        threshold_cm = NORDIC_SKI_THRESHOLD_CM) {
  weather_df |>
    filter(is_snow_season(date)) |>
    filter(is.na(source) | source != "forecast") |>
    filter(!is.na(snow_depth)) |>
    arrange(date) |>
    group_by(snow_year) |>
    mutate(cum_ski_days = cumsum(snow_depth >= threshold_cm)) |>
    ungroup() |>
    select(snow_year, doss, date, cum_ski_days)
}

# ── Daily snow-season series for year-by-year facet plot ─────────────────────
# Obs only; includes ski-day flag at park threshold.
compute_snow_season_daily <- function(weather_df,
                                      threshold_cm = NORDIC_SKI_THRESHOLD_CM) {
  weather_df |>
    filter(is_snow_season(date)) |>
    filter(is.na(source) | source != "forecast") |>
    mutate(
      snowfall   = coalesce(snowfall, 0),
      is_ski_day = !is.na(snow_depth) & snow_depth >= threshold_cm,
      # Fill NA depth as 0 for continuous area ribbons; ski dots use is_ski_day
      snow_depth = coalesce(snow_depth, 0),
      season_lab = paste0(snow_year, "\u2013", substr(as.character(snow_year + 1L), 3, 4))
    )
}

# ── Nordic ski index: days with snow_depth >= threshold ─────────────────────
compute_nordic_ski_index <- function(weather_df,
                                     threshold_cm = NORDIC_SKI_THRESHOLD_CM) {
  weather_df |>
    filter(is_snow_season(date)) |>
    filter(is.na(source) | source != "forecast") |>
    group_by(snow_year) |>
    summarise(
      skiable_days    = sum(!is.na(snow_depth) & snow_depth >= threshold_cm,
                            na.rm = TRUE),
      days_with_depth = sum(!is.na(snow_depth)),
      season_days     = n(),
      first_skiable   = {
        hit <- which(!is.na(snow_depth) & snow_depth >= threshold_cm)
        if (length(hit)) date[min(hit)] else as.Date(NA)
      },
      last_skiable    = {
        hit <- which(!is.na(snow_depth) & snow_depth >= threshold_cm)
        if (length(hit)) date[max(hit)] else as.Date(NA)
      },
      .groups = "drop"
    ) |>
    mutate(
      coverage = if_else(season_days > 0, days_with_depth / season_days, NA_real_),
      threshold_cm = threshold_cm
    )
}

# ── Current season summary ───────────────────────────────────────────────────
current_snow_summary <- function(weather_df,
                                 threshold_cm = NORDIC_SKI_THRESHOLD_CM) {
  current_year <- current_snow_year()

  obs <- weather_df |>
    filter(is.na(source) | source != "forecast")

  today_row <- obs |>
    filter(date <= Sys.Date()) |>
    slice_tail(n = 1)

  current_season <- obs |>
    filter(snow_year == current_year, is_snow_season(date), !is.na(snowfall)) |>
    arrange(date) |>
    mutate(cum_snowfall_cm = cumsum(coalesce(snowfall, 0)))

  peak_df <- compute_peak_snow_depth(obs, threshold_cm = threshold_cm)

  cum_so_far  <- if (nrow(current_season) > 0) max(current_season$cum_snowfall_cm) else 0
  depth_today <- coalesce(today_row$snow_depth[[1]], 0)

  # Percentile rank among completed historical seasons (same day of season)
  current_doss <- if (nrow(current_season) > 0) max(current_season$doss) else 0

  hist_at_same_doss <- compute_cumulative_snowfall(obs) |>
    filter(snow_year < current_year, doss <= current_doss) |>
    group_by(snow_year) |>
    slice_max(doss, n = 1) |>
    ungroup()

  snow_pct_rank <- if (nrow(hist_at_same_doss) > 0) {
    round(100 * mean(hist_at_same_doss$cum_snowfall_cm < cum_so_far))
  } else {
    NA_real_
  }

  # Nordic ski index (season-to-date + historical percentile at same DOSS)
  ski_idx <- compute_nordic_ski_index(obs, threshold_cm = threshold_cm)
  current_ski <- ski_idx |> filter(snow_year == current_year)
  skiable_days_so_far <- if (nrow(current_ski) > 0) current_ski$skiable_days[[1]] else 0L
  ski_coverage <- if (nrow(current_ski) > 0) current_ski$coverage[[1]] else NA_real_

  # Cumulative skiable days by DOSS for percentile vs prior winters
  ski_daily <- obs |>
    filter(is_snow_season(date), !is.na(snow_depth)) |>
    arrange(date) |>
    group_by(snow_year) |>
    mutate(cum_skiable = cumsum(snow_depth >= threshold_cm)) |>
    ungroup()

  hist_ski_at_doss <- ski_daily |>
    filter(snow_year < current_year, doss <= current_doss) |>
    group_by(snow_year) |>
    slice_max(doss, n = 1) |>
    ungroup()

  skiable_pct_rank <- if (nrow(hist_ski_at_doss) > 0) {
    round(100 * mean(hist_ski_at_doss$cum_skiable < skiable_days_so_far))
  } else {
    NA_real_
  }

  skiable_days_hist <- ski_idx |>
    filter(snow_year < current_year) |>
    pull(skiable_days)

  cond_score <- snow_to_condition_score(depth_today)

  list(
    snow_year             = current_year,
    depth_today_cm        = depth_today,
    cum_snowfall_cm       = cum_so_far,
    snow_pct_rank         = snow_pct_rank,
    skiable_days_so_far   = skiable_days_so_far,
    skiable_days_hist     = skiable_days_hist,
    skiable_pct_rank      = skiable_pct_rank,
    skiable_threshold_cm  = threshold_cm,
    skiable_coverage      = ski_coverage,
    skiable_partial_data  = !is.na(ski_coverage) && ski_coverage < 0.8,
    first_skiable         = if (nrow(current_ski) > 0) current_ski$first_skiable[[1]] else as.Date(NA),
    last_skiable          = if (nrow(current_ski) > 0) current_ski$last_skiable[[1]] else as.Date(NA),
    condition_score       = cond_score,
    condition_label       = score_to_label(cond_score),
    current_season        = current_season,
    peak_df               = peak_df,
    ski_index             = ski_idx
  )
}

# snow_oneliner() removed — use condition_oneliner(mode="snow", ...) from condition.R
