# R/bayes_trends.R
# Bayesian trend bands via brms (linear + spline) + trail annual summaries.
# Linear:  y ~ x
# Smooth:  y ~ s(x)   (thin-plate / mgcv spline in brms)
# Modest MCMC tuned for ~20–30 annual points (CI cost).
# Depends on: setup.R, parks_config.R, condition.R, mud columns on weather_df.

# Default MCMC budget for annual trend fits (documented for GHA).
# ~25 annual points: keep short to limit Quarto/GHA wall time; fits are
# cached under data/cache/brms (file_refit = "on_change").
.TP_BRMS_CHAINS  <- 2L
.TP_BRMS_ITER    <- 500L
.TP_BRMS_WARMUP  <- 250L

# Optional on-disk cache for compiled/fitted models (speeds re-renders).
.tp_brms_cache_dir <- function() {
  candidates <- c(
    file.path("data", "cache", "brms"),
    file.path("..", "..", "data", "cache", "brms"),
    file.path(tempdir(), "trailpulse_brms")
  )
  for (d in candidates[1:2]) {
    root <- dirname(dirname(d))
    if (dir.exists(root) || dir.exists(dirname(d))) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      if (dir.exists(d)) return(d)
    }
  }
  d <- candidates[[3]]
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.tp_brms_cache_file <- function(kind, x, y, data, seed) {
  payload <- list(
    kind = kind, x = x, y = y, seed = as.integer(seed),
    xy = data[order(data[[x]]), c(x, y), drop = FALSE]
  )
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(payload, tf)
  key <- unname(tools::md5sum(tf))
  file.path(.tp_brms_cache_dir(), paste0("tp_", kind, "_", key))
}

# Summarise posterior_epred draws → mean + CrI band.
.tp_epred_band <- function(fit, newdata, x_grid, level, method) {
  alpha <- (1 - level) / 2
  pe <- brms::posterior_epred(fit, newdata = newdata)
  tibble::tibble(
    x = x_grid,
    .mean = apply(pe, 2L, mean),
    .lower = apply(pe, 2L, stats::quantile, probs = alpha),
    .upper = apply(pe, 2L, stats::quantile, probs = 1 - alpha),
    method = method
  )
}

.tp_freq_lm_band <- function(data, x, y, x_grid, level) {
  fml <- stats::as.formula(paste(y, "~", x))
  fit <- stats::lm(fml, data = data)
  newdata <- data.frame(x_grid)
  names(newdata) <- x
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

.tp_freq_loess_band <- function(data, x, y, x_grid, level) {
  # Fallback only when brms smooth fails
  fml <- stats::as.formula(paste(y, "~", x))
  fit <- stats::loess(fml, data = data, span = 0.75)
  newdata <- data.frame(x_grid)
  names(newdata) <- x
  pred <- stats::predict(fit, newdata = newdata, se = TRUE)
  z <- stats::qnorm(1 - (1 - level) / 2)
  tibble::tibble(
    x = x_grid,
    .mean = as.numeric(pred$fit),
    .lower = as.numeric(pred$fit - z * pred$se.fit),
    .upper = as.numeric(pred$fit + z * pred$se.fit),
    method = "frequentist_loess"
  )
}

# Core brms fitter for annual series.
# kind = "lm" → y ~ x; kind = "smooth" → y ~ s(x)
.tp_brms_band <- function(data, x, y,
                          kind = c("lm", "smooth"),
                          x_grid = NULL,
                          level = 0.95,
                          seed = 123L,
                          chains = .TP_BRMS_CHAINS,
                          iter = .TP_BRMS_ITER,
                          warmup = .TP_BRMS_WARMUP) {
  kind <- match.arg(kind)
  data <- as.data.frame(data)
  x <- as.character(x)[[1]]
  y <- as.character(y)[[1]]
  data <- data[is.finite(data[[x]]) & is.finite(data[[y]]), , drop = FALSE]
  min_n <- if (identical(kind, "smooth")) 8L else 3L
  if (nrow(data) < min_n) return(NULL)

  if (is.null(x_grid)) {
    x_grid <- seq(min(data[[x]]), max(data[[x]]), length.out = 40L)
  }
  newdata <- data.frame(x_grid)
  names(newdata) <- x

  if (!requireNamespace("brms", quietly = TRUE)) {
    warning("brms not available; using frequentist fallback for ", kind, ".")
    return(if (identical(kind, "lm")) {
      .tp_freq_lm_band(data, x, y, x_grid, level)
    } else {
      .tp_freq_loess_band(data, x, y, x_grid, level)
    })
  }

  # Center predictor for numerical stability; keep original column name via offset
  x_mean <- mean(data[[x]])
  data$.x_c <- data[[x]] - x_mean
  newdata$.x_c <- x_grid - x_mean

  fml <- if (identical(kind, "lm")) {
    stats::as.formula(paste(y, "~ .x_c"))
  } else {
    # Modest basis for ~25 annual points (avoids overfit / slow sampling)
    k <- max(4L, min(6L, nrow(data) - 2L))
    stats::as.formula(paste0(y, " ~ s(.x_c, k = ", k, ")"))
  }

  cache_file <- .tp_brms_cache_file(kind, x, y, data[, c(x, y), drop = FALSE], seed)

  priors <- c(
    brms::prior(normal(0, 50), class = "Intercept"),
    brms::prior(student_t(3, 0, 10), class = "sigma")
  )
  if (identical(kind, "lm")) {
    priors <- c(brms::prior(normal(0, 10), class = "b"), priors)
  }

  fit <- tryCatch(
    suppressWarnings(brms::brm(
      formula = fml,
      data = data,
      family = stats::gaussian(),
      prior = priors,
      chains = chains,
      iter = iter,
      warmup = warmup,
      seed = as.integer(seed),
      refresh = 0,
      silent = 2,
      open_progress = FALSE,
      backend = "rstan",
      file = cache_file,
      file_refit = "on_change"
    )),
    error = function(e) {
      warning("brms ", kind, " failed (", conditionMessage(e), "); frequentist fallback.")
      NULL
    }
  )
  if (is.null(fit)) {
    return(if (identical(kind, "lm")) {
      .tp_freq_lm_band(data, x, y, x_grid, level)
    } else {
      .tp_freq_loess_band(data, x, y, x_grid, level)
    })
  }

  band <- tryCatch(
    .tp_epred_band(fit, newdata, x_grid, level,
                   method = if (identical(kind, "lm")) "brms_lm" else "brms_smooth"),
    error = function(e) {
      warning("posterior_epred failed; frequentist fallback.")
      NULL
    }
  )
  if (is.null(band)) {
    return(if (identical(kind, "lm")) {
      .tp_freq_lm_band(data, x, y, x_grid, level)
    } else {
      .tp_freq_loess_band(data, x, y, x_grid, level)
    })
  }
  band
}

bayes_lm_band <- function(data, x, y, x_grid = NULL, level = 0.95,
                          seed = 123L, ...) {
  .tp_brms_band(data, x, y, kind = "lm", x_grid = x_grid, level = level,
                seed = seed, ...)
}

bayes_smooth_band <- function(data, x, y, x_grid = NULL, level = 0.95,
                              seed = 123L, ...) {
  .tp_brms_band(data, x, y, kind = "smooth", x_grid = x_grid, level = level,
                seed = seed, ...)
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
