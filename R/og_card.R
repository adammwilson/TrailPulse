# R/og_card.R
# Generates a 1200×630 social-media card (Open Graph / Twitter) for a park page.
# Uses PNG icons from img/mudface/ for the condition illustration.
# Output saved to <output_path>; referenced via `image:` in the page YAML.
#
# Call once per render from the park's index.qmd fetch-data chunk.

# ── 4-level condition ranking ────────────────────────────────────────────────
# Maps a mud_level (0–10) or snow depth to one of four trail-condition tiers.
# Returns list(label, icon_num, icon_dir, rank_col).
#
# Snow depth breakpoints (cm):  Great ≥ 25 · Good ≥ 10 · Poor ≥ 5 · Awful < 5
# Mud level breakpoints (0-10): Great < 3  · Good < 5  · Poor < 7.5 · Awful ≥ 7.5

.trail_rank <- function(mud_condition_score = NULL, snow_condition_score = NULL) {
  has_snow <- !is.null(snow_condition_score) &&
               !is.na(snow_condition_score)  &&
               snow_condition_score >= 2

  score <- if (has_snow) {
    snow_condition_score
  } else {
    if (is.null(mud_condition_score) || is.na(mud_condition_score)) 5.0
    else mud_condition_score
  }
  idir  <- if (has_snow) "snowface" else "mudface"

  lbl    <- score_to_label(score)
  clr    <- condition_color(lbl)
  icon_n <- switch(lbl,
    "Avoid" = 4L, "Poor" = 4L, "Good" = 2L, "Great" = 1L
  )

  list(label = lbl, icon_num = icon_n, icon_dir = idir, rank_col = clr)
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


# ── Main card generator ──────────────────────────────────────────────────────

generate_og_card <- function(
    park_name            = "Hunters Creek Park",
    location             = "Wales, NY",
    mud_condition_score  = NULL,
    snow_condition_score = NULL,
    go                   = NULL,    # logical go/no-go; inferred from score if NULL
    temp_f               = NULL,    # today's high in °F
    precip_7d_mm         = NULL,
    forecast_summary     = NULL,
    output_path          = "og-card.png",
    width_px             = 1200,
    height_px            = 630
) {
  # ── Condition rank & icon ──────────────────────────────────────────────────
  rank      <- .trail_rank(mud_condition_score, snow_condition_score)
  lbl       <- rank$label
  state_col <- rank$rank_col

  icon_path <- file.path(.icon_dir(rank$icon_dir),
                         sprintf("%s%02d.png", rank$icon_dir, rank$icon_num))
  icon_img  <- png::readPNG(icon_path)

  lp       <- .logo_path()
  logo_img <- if (!is.null(lp)) png::readPNG(lp) else NULL

  # ── Active score (1–10) for slider ────────────────────────────────────────
  has_snow  <- !is.null(snow_condition_score) &&
               !is.na(snow_condition_score)   &&
               snow_condition_score >= 2
  score     <- pmax(1, pmin(10,
    if (has_snow) coalesce(snow_condition_score, 5)
    else          coalesce(mud_condition_score,  5)
  ))
  score_frac <- (score - 1) / 9   # 0–1 for slider fill

  # ── Go / no-go ────────────────────────────────────────────────────────────
  if (is.null(go)) go <- score >= 6

  # ── Colour palette ─────────────────────────────────────────────────────────
  # Full-card background: light green for go, light red for no-go
  bg_right  <- if (isTRUE(go)) "#d0f0db" else "#f0d0d0"
  go_col    <- if (isTRUE(go)) "#16a34a" else "#dc2626"
  go_txt    <- if (isTRUE(go)) "\u2705  GO" else "\u274c  NO-GO"

  # Dark accent colours for readability on light backgrounds
  acc_lkp   <- c(Avoid="#991b1b", Poor="#92400e", Fair="#78350f",
                 Good="#3f6212",  Great="#14532d")
  accent    <- acc_lkp[[lbl]]
  text_main <- if (isTRUE(go)) "#0d2418" else "#240d0d"
  text_sub  <- if (isTRUE(go)) "#2d5e3d" else "#5e2d2d"

  # Bright slider-fill colours (still readable on light bg)
  fill_lkp  <- c(Avoid="#f87171", Poor="#fbbf24", Fair="#fde047",
                 Good="#a3e635",  Great="#4ade80")
  fill_col  <- fill_lkp[[lbl]]

  # ── Text content ──────────────────────────────────────────────────────────
  date_txt   <- format(Sys.Date(), "%B %d, %Y")
  score_txt  <- sprintf("%.1f / 10", score)
  temp_txt   <- if (!is.null(temp_f) && !is.na(temp_f))
    sprintf("%.0f\u00b0F", as.numeric(temp_f)) else NULL
  precip_txt <- if (!is.null(precip_7d_mm) && !is.na(precip_7d_mm))
    sprintf("%.1f mm / %.1f in (7-day)", precip_7d_mm, precip_7d_mm / 25.4) else NULL
  fc_txt     <- if (!is.null(forecast_summary) && nzchar(forecast_summary))
    forecast_summary else NULL

  # ── Geometry ───────────────────────────────────────────────────────────────
  rx        <- 470    # right-panel x start
  rx_end    <- 1170   # right-panel x end
  bar_fill  <- rx + score_frac * (rx_end - rx)

  # ── Build plot ─────────────────────────────────────────────────────────────
  p <- ggplot() +

    # Full-card background (light go/no-go colour)
    annotate("rect", xmin=0, xmax=1200, ymin=0, ymax=630, fill=bg_right) +
    # Left accent stripe
    annotate("rect", xmin=0, xmax=7, ymin=0, ymax=630, fill=accent) +

    # Icon (renders on coloured background — PNG should have transparency)
    annotation_raster(icon_img, xmin=20, xmax=420, ymin=100, ymax=570) +

    # ── Right panel ────────────────────────────────────────────────────────

    # Park name
    annotate("text", x=rx, y=596, label=park_name,
             color=text_main, size=8.5, fontface="bold", hjust=0) +

    # Location · Date
    annotate("text", x=rx, y=552,
             label=paste0(location, "  \u2022  ", date_txt),
             color=text_sub, size=5.5, hjust=0) +

    # Divider
    annotate("segment", x=rx, xend=rx_end, y=522, yend=522,
             color=text_sub, linewidth=0.4, alpha=0.5) +

    # Section label
    annotate("text", x=rx, y=500, label="TRAIL CONDITIONS",
             color=text_sub, size=4.5, fontface="bold", hjust=0) +

    # Condition label (large, dark) + score right-aligned
    annotate("text", x=rx,     y=458, label=lbl,
             color=accent, size=12, fontface="bold", hjust=0) +
    annotate("text", x=rx_end, y=458, label=score_txt,
             color=accent, size=6.5, fontface="bold", hjust=1) +

    # ── Slider bar ──────────────────────────────────────────────────────────
    # Track
    annotate("rect", xmin=rx, xmax=rx_end, ymin=417, ymax=431,
             fill=text_main, alpha=0.20) +
    # Fill
    annotate("rect", xmin=rx, xmax=bar_fill, ymin=417, ymax=431,
             fill=fill_col, alpha=0.90) +
    # End-cap tick
    annotate("segment", x=bar_fill, xend=bar_fill, y=413, yend=435,
             color=text_main, linewidth=1.2) +
    # Scale labels
    annotate("text", x=rx,     y=405, label="Avoid",
             color=text_sub, size=3.2, hjust=0) +
    annotate("text", x=rx_end, y=405, label="Great",
             color=text_sub, size=3.2, hjust=1) +

    # ── Go / no-go badge ────────────────────────────────────────────────────
    annotate("text", x=rx, y=375, label=go_txt,
             color=go_col, size=7, fontface="bold", hjust=0) +

    # ── Stats ───────────────────────────────────────────────────────────────
    {if (!is.null(temp_txt))
      annotate("text", x=rx, y=335,
               label=paste0("\u26a1 High: ", temp_txt),
               color=text_main, size=5.5, hjust=0)} +
    {if (!is.null(precip_txt))
      annotate("text", x=rx, y=295,
               label=paste0("\U0001f327 ", precip_txt),
               color=text_main, size=5.5, hjust=0)} +

    # Divider
    annotate("segment", x=rx, xend=rx_end, y=265, yend=265,
             color=text_sub, linewidth=0.4, alpha=0.5) +

    # "7-DAY FORECAST" label
    annotate("text", x=rx, y=245, label="7-DAY FORECAST",
             color=text_sub, size=4.5, fontface="bold", hjust=0) +

    # Forecast text
    {if (!is.null(fc_txt))
      annotate("text", x=rx, y=203, label=fc_txt,
               color=accent, size=7.5, fontface="bold", hjust=0)} +

    # Brand: small logo + "TrailPulse" text side by side
    {if (!is.null(logo_img))
      annotation_raster(logo_img, xmin=rx, xmax=rx+46, ymin=20, ymax=66)} +
    annotate("text", x=rx+54, y=45, label="TrailPulse",
             color=accent, size=8.5, fontface="bold", hjust=0) +

    coord_cartesian(xlim=c(0, 1200), ylim=c(0, 630), expand=FALSE) +
    theme_void()

  dir.create(dirname(output_path), showWarnings=FALSE, recursive=TRUE)
  ggsave(output_path, p,
         width  = width_px  / 100,
         height = height_px / 100,
         dpi    = 100,
         bg     = bg_right)

  invisible(output_path)
}
