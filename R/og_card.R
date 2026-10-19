# R/og_card.R
# Generates a 1200×630 social-media card (Open Graph / Twitter) for a park page.
# Score-forward layout: large Avoid→Great label + numeric score + color scale.
# Optional mudface/snowface PNGs are secondary (corner accent) and easy to disable.
# Overwrites stable og-card.png each render.
#
# Snow mode: Dec–Mar calendar window, or snow on the ground (>2 cm) otherwise.
# Stats: trail → high °F, 7-day precip, trend; snow → depth, 48h snowfall, low/high °F.
# No GO/NO-GO — Avoid→Great scale only.

# ── Mode selection (locked prefs) ────────────────────────────────────────────
# Dec–Mar OR snow depth > 2 cm → snow card; else trail.
og_card_mode <- function(date = Sys.Date(), snow_depth_cm = 0) {
  m <- as.integer(format(as.Date(date), "%m"))
  in_dec_mar <- m %in% c(12L, 1L, 2L, 3L)
  depth <- suppressWarnings(as.numeric(snow_depth_cm))
  has_snow <- !is.na(depth) && depth > 2
  if (in_dec_mar || has_snow) "snow" else "trail"
}

# ── Condition rank & optional face asset for an explicit mode ────────────────
.og_rank <- function(mode, condition_score) {
  score <- if (is.null(condition_score) || is.na(condition_score)) 5.0
           else as.numeric(condition_score)
  score <- pmax(1, pmin(10, score))
  idir  <- if (identical(mode, "snow")) "snowface" else "mudface"
  lbl   <- score_to_label(score)
  if (is.na(lbl)) lbl <- "Poor"
  clr   <- condition_color(lbl)
  icon_n <- if      (score >= 8) 1L
            else if (score >= 6) 2L
            else if (score >= 2) 3L
            else                 4L
  list(label = lbl, icon_num = icon_n, icon_dir = idir,
       rank_col = clr, score = score)
}

# Locate img/<subdir>/ from either the project root or a parks/*/ subdirectory.
.icon_dir <- function(subdir) {
  sentinel <- sprintf("%s01.png", subdir)
  candidates <- c(file.path("img", subdir), file.path("../../img", subdir))
  for (d in candidates) {
    if (file.exists(file.path(d, sentinel))) return(d)
  }
  stop("Cannot locate img/", subdir, "/ directory")
}

# Locate img/logo.png from either the project root or a parks/*/ subdirectory.
.logo_path <- function() {
  candidates <- c(file.path("img", "logo.png"), file.path("../../img", "logo.png"))
  for (p in candidates) if (file.exists(p)) return(p)
  NULL
}

.html_escape_attr <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# Build one-line OG title / description from the same inputs as the card.
og_meta_text <- function(park_name, mode, label, score,
                         temp_high_f = NULL, temp_low_f = NULL,
                         precip_7d_mm = NULL, forecast_summary = NULL,
                         snow_depth_cm = NULL, snowfall_48h_cm = NULL,
                         as_of = Sys.Date()) {
  score_n <- if (is.null(score) || is.na(score)) NA_real_ else as.numeric(score)
  score_txt <- if (!is.na(score_n)) sprintf("%.1f", score_n) else "?"
  title <- sprintf("%s \u2014 %s (%s/10)", park_name, label, score_txt)
  date_short <- format(as_of, "%b %d, %Y")

  if (identical(mode, "snow")) {
    depth <- if (!is.null(snow_depth_cm) && !is.na(snow_depth_cm))
      sprintf("%.0f cm depth", as.numeric(snow_depth_cm)) else NULL
    sf48 <- if (!is.null(snowfall_48h_cm) && !is.na(snowfall_48h_cm))
      sprintf("%.0f cm snow (48h)", as.numeric(snowfall_48h_cm)) else NULL
    lo <- if (!is.null(temp_low_f) && !is.na(temp_low_f))
      sprintf("Low %.0f\u00b0F", as.numeric(temp_low_f)) else NULL
    hi <- if (!is.null(temp_high_f) && !is.na(temp_high_f))
      sprintf("High %.0f\u00b0F", as.numeric(temp_high_f)) else NULL
    bits <- c("Snow", depth, sf48, lo, hi, paste("Updated", date_short))
  } else {
    hi <- if (!is.null(temp_high_f) && !is.na(temp_high_f))
      sprintf("High %.0f\u00b0F", as.numeric(temp_high_f)) else NULL
    precip <- if (!is.null(precip_7d_mm) && !is.na(precip_7d_mm))
      sprintf("%.0f mm rain (7d)", as.numeric(precip_7d_mm)) else NULL
    trend <- if (!is.null(forecast_summary) && nzchar(forecast_summary) &&
                 !identical(forecast_summary, "Forecast unavailable"))
      forecast_summary else NULL
    bits <- c("Trail conditions", hi, precip, trend, paste("Updated", date_short))
  }

  description <- paste(bits[!vapply(bits, is.null, logical(1))], collapse = " \u00b7 ")
  list(title = title, description = description)
}

# Write park-local meta overrides for Quarto include-in-header (stable path).
write_og_meta <- function(title, description, path = "og-meta.html") {
  lines <- c(
    sprintf('<meta property="og:title" content="%s" />',
            .html_escape_attr(title)),
    sprintf('<meta property="og:description" content="%s" />',
            .html_escape_attr(description)),
    sprintf('<meta name="twitter:title" content="%s" />',
            .html_escape_attr(title)),
    sprintf('<meta name="twitter:description" content="%s" />',
            .html_escape_attr(description)),
    sprintf('<meta name="description" content="%s" />',
            .html_escape_attr(description))
  )
  writeLines(lines, path, useBytes = FALSE)
  invisible(path)
}


# ── Main card generator ──────────────────────────────────────────────────────
# Layout priority: park name → huge condition label + score → color bar →
# secondary stats (temp / season) → brand. Face PNGs are optional corner accents.

generate_og_card <- function(
    park_name            = "Hunters Creek Park",
    location             = "Wales, NY",
    mode                 = c("trail", "snow"),
    condition_score      = NULL,
    temp_high_f          = NULL,
    temp_low_f           = NULL,
    precip_7d_mm         = NULL,
    forecast_summary     = NULL,
    snow_depth_cm        = NULL,
    snowfall_48h_cm      = NULL,
    show_face            = TRUE,   # FALSE = score-only (easy swap-out of cartoons)
    # Deprecated aliases
    mud_condition_score  = NULL,
    snow_condition_score = NULL,
    go                   = NULL,
    temp_f               = NULL,
    output_path          = "og-card.png",
    width_px             = 1200,
    height_px            = 630
) {
  mode <- match.arg(mode)

  if (is.null(condition_score)) {
    condition_score <- if (identical(mode, "snow"))
      snow_condition_score else mud_condition_score
  }
  if (is.null(temp_high_f) && !is.null(temp_f)) temp_high_f <- temp_f

  rank       <- .og_rank(mode, condition_score)
  lbl        <- rank$label
  score      <- rank$score
  score_frac <- (score - 1) / 9

  icon_img <- NULL
  if (isTRUE(show_face)) {
    icon_path <- file.path(
      .icon_dir(rank$icon_dir),
      sprintf("%s%02d.png", rank$icon_dir, rank$icon_num)
    )
    if (file.exists(icon_path)) icon_img <- png::readPNG(icon_path)
  }

  lp       <- .logo_path()
  logo_img <- if (!is.null(lp)) png::readPNG(lp) else NULL

  # Mode-tinted wash; accent from condition label (not go/no-go)
  if (identical(mode, "snow")) {
    bg_card   <- "#e8eef5"
    text_main <- "#0f1c2e"
    text_sub  <- "#3d5270"
  } else {
    bg_card   <- "#eef5ea"
    text_main <- "#0d2418"
    text_sub  <- "#2d5e3d"
  }

  acc_lkp <- c(Avoid = "#991b1b", Poor = "#92400e", Fair = "#78350f",
               Good = "#3f6212", Great = "#14532d")
  accent  <- if (!is.null(acc_lkp[[lbl]]) && !is.na(acc_lkp[[lbl]]))
    acc_lkp[[lbl]] else "#1f2937"

  # Segmented Avoid→Great scale colors (left→right)
  scale_cols <- c("#ef4444", "#f59e0b", "#84cc16", "#22c55e")
  fill_lkp   <- c(Avoid = "#f87171", Poor = "#fbbf24", Fair = "#fde047",
                  Good = "#a3e635", Great = "#4ade80")
  fill_col   <- if (!is.null(fill_lkp[[lbl]]) && !is.na(fill_lkp[[lbl]]))
    fill_lkp[[lbl]] else "#84cc16"

  date_txt   <- format(Sys.Date(), "%B %d, %Y")
  score_txt  <- sprintf("%.1f", score)
  mode_label <- if (identical(mode, "snow")) "SNOW" else "TRAIL"

  # Secondary stats (no emoji)
  if (identical(mode, "snow")) {
    depth_txt <- if (!is.null(snow_depth_cm) && !is.na(snow_depth_cm))
      sprintf("Depth %.0f cm", as.numeric(snow_depth_cm)) else NULL
    sf_txt <- if (!is.null(snowfall_48h_cm) && !is.na(snowfall_48h_cm))
      sprintf("48h snow %.0f cm", as.numeric(snowfall_48h_cm)) else NULL
    lo_txt <- if (!is.null(temp_low_f) && !is.na(temp_low_f))
      sprintf("Low %.0f\u00b0F", as.numeric(temp_low_f)) else NULL
    hi_txt <- if (!is.null(temp_high_f) && !is.na(temp_high_f))
      sprintf("High %.0f\u00b0F", as.numeric(temp_high_f)) else NULL
    stat1 <- paste(c(depth_txt, sf_txt), collapse = "  \u00b7  ")
    if (!nzchar(stat1)) stat1 <- NULL
    stat2 <- paste(c(lo_txt, hi_txt), collapse = "  \u00b7  ")
    if (!nzchar(stat2)) stat2 <- NULL
  } else {
    hi_txt <- if (!is.null(temp_high_f) && !is.na(temp_high_f))
      sprintf("High %.0f\u00b0F", as.numeric(temp_high_f)) else NULL
    precip_txt <- if (!is.null(precip_7d_mm) && !is.na(precip_7d_mm))
      sprintf("%.0f mm rain (7d)", as.numeric(precip_7d_mm)) else NULL
    stat1 <- paste(c(hi_txt, precip_txt), collapse = "  \u00b7  ")
    if (!nzchar(stat1)) stat1 <- NULL
    stat2 <- if (!is.null(forecast_summary) && nzchar(forecast_summary))
      forecast_summary else NULL
  }

  meta <- og_meta_text(
    park_name        = park_name,
    mode             = mode,
    label            = lbl,
    score            = score,
    temp_high_f      = temp_high_f,
    temp_low_f       = temp_low_f,
    precip_7d_mm     = precip_7d_mm,
    forecast_summary = forecast_summary,
    snow_depth_cm    = snow_depth_cm,
    snowfall_48h_cm  = snowfall_48h_cm
  )

  # Geometry — score block centered; face tucked top-right if present
  mx      <- 80     # left margin for primary content
  mx_end  <- 1120
  bar_y0  <- 300
  bar_y1  <- 328
  bar_fill <- mx + score_frac * (mx_end - mx)
  seg_w   <- (mx_end - mx) / 4

  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1200, ymin = 0, ymax = 630, fill = bg_card) +
    annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 630, fill = accent) +

    # Park identity
    annotate("text", x = mx, y = 580, label = park_name,
             color = text_main, size = 9, fontface = "bold", hjust = 0) +
    annotate("text", x = mx, y = 538,
             label = paste0(location, "  \u2022  ", date_txt),
             color = text_sub, size = 5.2, hjust = 0) +
    annotate("text", x = mx, y = 500, label = mode_label,
             color = text_sub, size = 4.2, fontface = "bold", hjust = 0) +

    # Primary: condition label (hero) + numeric score
    annotate("text", x = mx, y = 430, label = toupper(lbl),
             color = accent, size = 22, fontface = "bold", hjust = 0) +
    annotate("text", x = mx_end, y = 430,
             label = paste0(score_txt, " / 10"),
             color = accent, size = 14, fontface = "bold", hjust = 1) +

    # Clear 4-stop color scale track + fill + marker
    annotate("rect", xmin = mx, xmax = mx + seg_w, ymin = bar_y0, ymax = bar_y1,
             fill = scale_cols[[1]], alpha = 0.35) +
    annotate("rect", xmin = mx + seg_w, xmax = mx + 2 * seg_w,
             ymin = bar_y0, ymax = bar_y1, fill = scale_cols[[2]], alpha = 0.35) +
    annotate("rect", xmin = mx + 2 * seg_w, xmax = mx + 3 * seg_w,
             ymin = bar_y0, ymax = bar_y1, fill = scale_cols[[3]], alpha = 0.35) +
    annotate("rect", xmin = mx + 3 * seg_w, xmax = mx_end,
             ymin = bar_y0, ymax = bar_y1, fill = scale_cols[[4]], alpha = 0.35) +
    annotate("rect", xmin = mx, xmax = bar_fill, ymin = bar_y0, ymax = bar_y1,
             fill = fill_col, alpha = 0.95) +
    annotate("segment", x = bar_fill, xend = bar_fill,
             y = bar_y0 - 8, yend = bar_y1 + 8,
             color = text_main, linewidth = 1.6) +
    annotate("text", x = mx, y = 275, label = "Avoid",
             color = text_sub, size = 4, hjust = 0) +
    annotate("text", x = mx_end, y = 275, label = "Great",
             color = text_sub, size = 4, hjust = 1) +

    # Secondary stats
    { if (!is.null(stat1))
        annotate("text", x = mx, y = 220, label = stat1,
                 color = text_main, size = 6.2, hjust = 0) } +
    { if (!is.null(stat2))
        annotate("text", x = mx, y = 175, label = stat2,
                 color = text_main, size = 6.0, hjust = 0) } +

    # Optional small face accent (secondary; omit with show_face = FALSE)
    { if (!is.null(icon_img))
        annotation_raster(icon_img, xmin = 980, xmax = 1160, ymin = 470, ymax = 610) } +

    # Brand
    { if (!is.null(logo_img))
        annotation_raster(logo_img, xmin = mx, xmax = mx + 46, ymin = 28, ymax = 74) } +
    annotate("text", x = mx + 54, y = 52, label = "TrailPulse",
             color = accent, size = 8.5, fontface = "bold", hjust = 0) +

    coord_cartesian(xlim = c(0, 1200), ylim = c(0, 630), expand = FALSE) +
    theme_void()

  out_dir <- dirname(output_path)
  if (!identical(out_dir, ".") && !identical(out_dir, "")) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  ggsave(output_path, p,
         width  = width_px  / 100,
         height = height_px / 100,
         dpi    = 100,
         bg     = bg_card)

  invisible(list(
    path        = output_path,
    mode        = mode,
    label       = lbl,
    score       = score,
    title       = meta$title,
    description = meta$description
  ))
}
