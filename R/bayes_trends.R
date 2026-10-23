# R/bayes_trends.R
# Bayesian linear trend bands (rstanarm) + trail/mud annual trend summaries.
# Loess remains frequentist (geom_smooth); document in plot captions.
# Depends on: setup.R, parks_config.R, condition.R, mud columns on weather_df.

# ── Bayesian lm mean band via rstanarm::stan_glm ─────────────────────────────
# Returns tibble: x, .mean, .lower, .upper (95% CrI for E(y|x)).
# Falls back to frequentist lm CI if rstanarm is unavailable or fit fails.
bayes_lm_band <- function(data, x, y,
                          x_grid = NULL,
                          level = 0.95,
                          seed = 123L,
                          chains = 4L,
                          iter = 2000L,
                          warmup = 1000L) {
  data <- as.data.frame(data)
  x <- as.character(x)[[1]]
  y <- as.character(y)[[1]]
  data <- data[is.finite(data[[x]]) & is.finite(data[[y]]), , drop = FALSE]
  if (nrow(data) < 3L) return(NULL)

  if (is.null(x_grid)) {
    x_grid <- seq(min(data[[x]]), max(data[[x]]), length.out = 40L)
  }
  newdata <- data.frame(x_grid)
  names(newdata) <- x

  alpha <- (1 - level) / 2

  fit_freq_fallback <- function() {
    fml <- stats::as.formula(paste(y, "~", x))
    fit <- stats::lm(fml, data = data)
    pred <- stats::predict(fit, newdata = newdata, interval = "confidence",
                           level = level)
    tibble::tibble(
      x = x_grid,
      .mean = as.numeric(pred[, "fit"]),
      .lower = as.numeric(pred[, "lwr"]),
      .upper = as.numeric(pred[, "upr"]),
      method = "frequentist_lm"
    )
  }

  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    warning("rstanarm not available; using frequentist lm CI for trend band.")
    return(fit_freq_fallback())
  }

  fml <- stats::as.formula(paste(y, "~", x))
  fit <- tryCatch(
    rstanarm::stan_glm(
      fml,
      data = data,
      family = stats::gaussian(),
      prior = rstanarm::normal(0, 10),
      prior_intercept = rstanarm::normal(0, 50),
      prior_aux = rstanarm::exponential(1),
      chains = chains,
      iter = iter,
      warmup = warmup,
      seed = seed,
      refresh = 0,
      open_progress = FALSE
    ),
    error = function(e) {
      warning("stan_glm failed (", conditionMessage(e), "); frequentist fallback.")
      NULL
    }
  )
  if (is.null(fit)) return(fit_freq_fallback())

  pe <- tryCatch(
    rstanarm::posterior_epred(fit, newdata = newdata),
    error = function(e) {
      warning("posterior_epred failed; frequentist fallback.")
      NULL
    }
  )
  if (is.null(pe)) return(fit_freq_fallback())

  tibble::tibble(
    x = x_grid,
    .mean = apply(pe, 2L, mean),
    .lower = apply(pe, 2L, stats::quantile, probs = alpha),
    .upper = apply(pe, 2L, stats::quantile, probs = 1 - alpha),
    method = "rstanarm"
  )
}

# ── Shared soil-wetness composite (matches mud_model weights) ────────────────
.trail_soil_wetness <- function(df) {
  dplyr::mutate(
    df,
    soil_wetness = 0.50 * .data$soil_moisture_0_1 +
      0.35 * dplyr::coalesce(.data$soil_moisture_1_3, .data$soil_moisture_0_1) +
      0.15 * dplyr::coalesce(.data$soil_moisture_3_9, .data$soil_moisture_0_1)
  )
}

# ── Annual Good / Great trail days (calendar year) ───────────────────────────
# Labels from CONDITION_BREAKS (2, 6, 8): Good = [6, 8), Great = [8, Inf).
compute_annual_good_great_days <- function(weather_df) {
  weather_df |>
    dplyr::filter(is.na(.data$source) | .data$source != "forecast") |>
    dplyr::filter(!is.na(.data$condition_score)) |>
    dplyr::mutate(
      cal_year = lubridate::year(.data$date),
      is_good  = .data$condition_score >= 6 & .data$condition_score < 8,
      is_great = .data$condition_score >= 8
    ) |>
    dplyr::group_by(.data$cal_year) |>
    dplyr::summarise(
      good_days  = sum(.data$is_good, na.rm = TRUE),
      great_days = sum(.data$is_great, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$cal_year >= lubridate::year(WEATHER_START))
}

# ── Mean JJA soil moisture by calendar year ──────────────────────────────────
compute_jja_soil_moisture <- function(weather_df) {
  weather_df |>
    dplyr::filter(is.na(.data$source) | .data$source != "forecast") |>
    dplyr::filter(lubridate::month(.data$date) %in% 6:8) |>
    .trail_soil_wetness() |>
    dplyr::filter(!is.na(.data$soil_wetness)) |>
    dplyr::mutate(cal_year = lubridate::year(.data$date)) |>
    dplyr::group_by(.data$cal_year) |>
    dplyr::summarise(
      mean_jja_sm = mean(.data$soil_wetness, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$cal_year >= lubridate::year(WEATHER_START))
}

# ── First / last date of calendar-year runs with score > 6 for ≥7 days ───────
# Season = calendar year (same as trail YTD precip). A qualifying run is a
# contiguous stretch of days with condition_score > 6 lasting ≥ streak_n days.
# first_doy = start of the first such run; last_doy = end of the last such run.
compute_score_streak_bounds <- function(weather_df,
                                        score_gt = 6,
                                        streak_n = 7L) {
  daily <- weather_df |>
    dplyr::filter(is.na(.data$source) | .data$source != "forecast") |>
    dplyr::filter(!is.na(.data$condition_score)) |>
    dplyr::mutate(cal_year = lubridate::year(.data$date)) |>
    dplyr::filter(.data$cal_year >= lubridate::year(WEATHER_START)) |>
    dplyr::arrange(.data$date)

  if (nrow(daily) == 0) {
    return(tibble::tibble(
      cal_year = integer(), first_doy = integer(), last_doy = integer()
    ))
  }

  daily |>
    dplyr::group_by(.data$cal_year) |>
    dplyr::group_modify(function(g, key) {
      g <- dplyr::arrange(g, .data$date)
      ok <- !is.na(g$condition_score) & g$condition_score > score_gt
      if (!any(ok)) {
        return(tibble::tibble(first_doy = NA_integer_, last_doy = NA_integer_))
      }
      # New run when ok flips FALSE→TRUE
      run_start <- ok & !c(FALSE, ok[-length(ok)])
      run_id <- cumsum(run_start)
      run_id[!ok] <- 0L
      runs <- tibble::tibble(
        date = g$date,
        run_id = run_id
      ) |>
        dplyr::filter(.data$run_id > 0L) |>
        dplyr::group_by(.data$run_id) |>
        dplyr::summarise(
          n = dplyr::n(),
          start = min(.data$date),
          end = max(.data$date),
          .groups = "drop"
        ) |>
        dplyr::filter(.data$n >= streak_n)
      if (nrow(runs) == 0) {
        return(tibble::tibble(first_doy = NA_integer_, last_doy = NA_integer_))
      }
      tibble::tibble(
        first_doy = as.integer(lubridate::yday(min(runs$start))),
        last_doy  = as.integer(lubridate::yday(max(runs$end)))
      )
    }) |>
    dplyr::ungroup()
}
