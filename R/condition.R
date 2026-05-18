# R/condition.R
# Unified 1–10 trail condition scoring, 5-tier labelling, and go recommendations.
# Source after setup.R (requires tidyverse/dplyr, lubridate, glue).

# ── Constants ────────────────────────────────────────────────────────────────
CONDITION_BREAKS   <- c(2, 4, 6, 8)
CONDITION_LABELS_5 <- c("Avoid", "Poor", "Fair", "Good", "Great")

# Colours match og_card.R's existing 4-tier palette, extended to 5 tiers.
CONDITION_COLORS_5 <- c(
  "Avoid" = "#ef4444",
  "Poor"  = "#f59e0b",
  "Fair"  = "#eab308",
  "Good"  = "#84cc16",
  "Great" = "#22c55e"
)

# ── Label from score (vectorised) ────────────────────────────────────────────
# Maps a numeric score in [1, 10] to one of the 5 condition labels.
score_to_label <- function(score) {
  ifelse(
    is.na(score), NA_character_,
    CONDITION_LABELS_5[findInterval(score, c(-Inf, CONDITION_BREAKS, Inf))]
  )
}

# ── Mud condition score (vectorised) ─────────────────────────────────────────
# Inverts the internal 0–10 mud_score (0 = dry, 10 = impassable) to a
# user-facing 1–10 condition_score (1 = worst, 10 = ideal).
# Frozen ground (soil temp held below 0 for several days) gets 5.0 (Fair).
mud_to_condition_score <- function(mud_score, frozen = FALSE) {
  dplyr::case_when(
    frozen           ~ 5.0,
    is.na(mud_score) ~ NA_real_,
    TRUE             ~ round(pmax(1, pmin(10, 11 - mud_score)), 1)
  )
}

# ── Snow condition score (vectorised) ────────────────────────────────────────
# Piecewise-linear mapping from snow depth (cm) to condition score [1, 10].
# Anchors: 0 cm → 1, 5 cm → 3, 10 cm → 5, 20 cm → 7, 30 cm → 9, 60+ cm → 10.
snow_to_condition_score <- function(depth_cm) {
  d <- coalesce(as.numeric(depth_cm), 0)
  score <- dplyr::case_when(
    d <= 0  ~ 1.0,
    d <= 5  ~ 1.0 + (d / 5)         * 2.0,
    d <= 10 ~ 3.0 + ((d - 5)  / 5)  * 2.0,
    d <= 20 ~ 5.0 + ((d - 10) / 10) * 2.0,
    d <= 30 ~ 7.0 + ((d - 20) / 10) * 2.0,
    TRUE    ~ pmin(9.0 + (d - 30) / 30, 10.0)
  )
  round(score, 1)
}

# ── Go recommendation (vectorised) ───────────────────────────────────────────
# Computes a logical go/no-go flag independently of the condition label.
#   mode = "mud"  → go if condition_score >= 6
#   mode = "snow" → go if condition_score >= 6 (xc_ski) or >= 4 (snowshoe)
go_recommendation <- function(condition_score, mode, activity = "hiking") {
  score_ok <- !is.na(condition_score)
  if (mode == "mud")                              return(score_ok & condition_score >= 6)
  if (mode == "snow" && activity == "xc_ski")     return(score_ok & condition_score >= 6)
  if (mode == "snow" && activity == "snowshoe")   return(score_ok & condition_score >= 4)
  score_ok & condition_score >= 6   # safe default
}

# ── Color lookup ─────────────────────────────────────────────────────────────
condition_color <- function(label) {
  unname(CONDITION_COLORS_5[label] %||% "#888888")
}

# ── Historical condition percentile rank ─────────────────────────────────────
# Returns list(pct_rank, avg_score, relative_label) for the current date
# vs the same day-of-year across all historical years.
historical_condition_rank <- function(df,
                                      score_col = "condition_score",
                                      doy_col   = "dowy") {
  today_year <- lubridate::year(Sys.Date())
  doy_now    <- lubridate::yday(Sys.Date())

  score_now <- df |>
    dplyr::filter(date == Sys.Date()) |>
    dplyr::pull(!!dplyr::sym(score_col))
  score_now <- if (length(score_now) > 0) score_now[[1]] else NA_real_

  hist_scores <- df |>
    dplyr::filter(lubridate::year(date) < today_year,
                  lubridate::yday(date) == doy_now) |>
    dplyr::pull(!!dplyr::sym(score_col))

  if (length(hist_scores) == 0 || is.na(score_now)) {
    return(list(pct_rank       = NA_real_,
                avg_score      = NA_real_,
                relative_label = NA_character_))
  }

  avg_score <- round(mean(hist_scores, na.rm = TRUE), 1)
  pct_rank  <- round(100 * mean(hist_scores < score_now, na.rm = TRUE))

  relative_label <- dplyr::case_when(
    score_now > avg_score + 1.5 ~ "much better than average",
    score_now > avg_score + 0.5 ~ "better than average",
    score_now < avg_score - 1.5 ~ "much worse than average",
    score_now < avg_score - 0.5 ~ "worse than average",
    TRUE                         ~ "about average"
  )

  list(pct_rank = pct_rank, avg_score = avg_score, relative_label = relative_label)
}

# ── Unified condition one-liner ───────────────────────────────────────────────
# Produces a single human-readable sentence summarising trail conditions.
# mode = "mud" or "snow".
condition_oneliner <- function(mode, score, label, go_recommendation,
                               activities = c("hiking"),
                               depth_cm   = NULL,
                               pct_rank   = NULL,
                               frozen     = FALSE) {
  if (is.na(label)) return("Condition data unavailable.")

  if (mode == "mud") {
    if (isTRUE(frozen)) {
      return("Surface is frozen \u2014 watch for ice on shaded sections.")
    }
    base <- switch(label,
      "Avoid" = "Trails are impassable \u2014 please stay home to protect trail surfaces.",
      "Poor"  = "Trails are muddy \u2014 consider staying on hard-packed surfaces.",
      "Fair"  = "Trails are softening \u2014 low areas may have mud.",
      "Good"  = if (!is.na(score) && score >= 7)
                  "Trails are firm with good traction \u2014 ideal conditions."
                else
                  "Trails are tacky \u2014 expect excellent grip, light mud on low sections.",
      "Great" = if (!is.na(score) && score >= 9.5)
                  "Trails are bone dry. Perfect conditions."
                else
                  "Trails are dry and firm \u2014 great day to hike or ride.",
      "Condition data unavailable."
    )
    if ("mountain_biking" %in% activities && label %in% c("Avoid", "Poor")) {
      base <- paste(base, "\U1F6B5 Mountain biking is strongly discouraged.")
    }
    return(base)
  }

  if (mode == "snow") {
    depth_str <- if (!is.null(depth_cm) && !is.na(depth_cm))
      glue::glue("{round(depth_cm)} cm") else "some snow"
    base <- switch(label,
      "Avoid" = "No snow on the ground currently.",
      "Poor"  = glue::glue("Just a light dusting ({depth_str}) \u2014 not yet skiable."),
      "Fair"  = glue::glue("{depth_str} on the ground \u2014 skiable on groomed trails."),
      "Good"  = glue::glue("{depth_str} of snow \u2014 good Nordic ski conditions."),
      "Great" = glue::glue("{depth_str} of snow \u2014 powder day! Get out there!"),
      "No snow data."
    )
    if (!is.null(pct_rank) && !is.na(pct_rank) &&
        !is.null(depth_cm) && !is.na(depth_cm) && depth_cm > 0) {
      base <- glue::glue("{base} More snowfall than {pct_rank}% of seasons on record.")
    }
    return(base)
  }

  "Condition data unavailable."
}
