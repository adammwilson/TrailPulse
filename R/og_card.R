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

.trail_rank <- function(mud_level = NULL, depth_cm = NULL) {
  has_snow <- !is.null(depth_cm) && !is.na(depth_cm) && depth_cm > 2
  if (has_snow) {
    if      (depth_cm >= 25) list(label = "Great", icon_num = 1L, icon_dir = "snowface", rank_col = "#22c55e")
    else if (depth_cm >= 10) list(label = "Good",  icon_num = 2L, icon_dir = "snowface", rank_col = "#84cc16")
    else if (depth_cm >=  5) list(label = "Poor",  icon_num = 3L, icon_dir = "snowface", rank_col = "#f59e0b")
    else                     list(label = "Awful", icon_num = 4L, icon_dir = "snowface", rank_col = "#ef4444")
  } else {
    ml <- if (is.null(mud_level) || is.na(mud_level)) 5 else mud_level
    if      (ml < 3)   list(label = "Great", icon_num = 1L, icon_dir = "mudface", rank_col = "#22c55e")
    else if (ml < 5)   list(label = "Good",  icon_num = 2L, icon_dir = "mudface", rank_col = "#84cc16")
    else if (ml < 7.5) list(label = "Poor",  icon_num = 3L, icon_dir = "mudface", rank_col = "#f59e0b")
    else               list(label = "Awful", icon_num = 4L, icon_dir = "mudface", rank_col = "#ef4444")
  }
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
    park_name        = "Hunters Creek Park",
    location         = "Elma, NY",
    mud_level        = NULL,
    mud_name         = NULL,
    temp_c           = NULL,
    precip_7d_mm     = NULL,
    depth_cm         = NULL,
    forecast_summary = NULL,   # e.g. "Getting drier", "Staying wet"
    output_path      = "og-card.png",
    width_px         = 1200,
    height_px        = 630
) {
  has_snow <- !is.null(depth_cm) && !is.na(depth_cm) && depth_cm > 2

  # ── Condition rank & icon ──────────────────────────────────────────────────
  rank      <- .trail_rank(mud_level, depth_cm)
  icon_path <- file.path(.icon_dir(rank$icon_dir),
                         sprintf("%s%02d.png", rank$icon_dir, rank$icon_num))
  icon_img  <- png::readPNG(icon_path)

  # ── Logo (optional) ───────────────────────────────────────────────────────
  lp        <- .logo_path()
  logo_img  <- if (!is.null(lp)) png::readPNG(lp) else NULL

  # ── Colour scheme (4 levels: Great / Good / Poor / Awful) ──────────────────
  rank_num  <- rank$icon_num
  bg_dark   <- c("#0d1b0f", "#1a2a0a", "#2a1e0a", "#1a0505")[rank_num]
  accent    <- c("#4ade80", "#a3e635", "#fbbf24", "#f87171")[rank_num]
  text_main <- "#f8fafc"
  text_sub  <- c("#86efac", "#bef264", "#fde68a", "#fecaca")[rank_num]
  state_col <- rank$rank_col

  # ── Text elements ──────────────────────────────────────────────────────────
  cond_txt   <- rank$label
  date_txt   <- format(Sys.Date(), "%B %d, %Y")
  temp_txt   <- if (!is.null(temp_c) && !is.na(temp_c))
    sprintf("%.0f\u00b0C / %.0f\u00b0F", temp_c, temp_c * 9/5 + 32) else NULL
  precip_txt <- if (!is.null(precip_7d_mm) && !is.na(precip_7d_mm))
    sprintf("%.1f mm rain past 7 days", precip_7d_mm) else NULL
  fc_txt     <- if (!is.null(forecast_summary) && nzchar(forecast_summary))
    forecast_summary else NULL

  # ── Right-panel x origin ───────────────────────────────────────────────────
  rx <- 470

  # ── Build plot ─────────────────────────────────────────────────────────────
  p <- ggplot() +

    # Backgrounds: dark right, white left for clean icon rendering
    annotate("rect", xmin=0,   xmax=1200, ymin=0, ymax=630, fill=bg_dark) +
    annotate("rect", xmin=0,   xmax=440,  ymin=0, ymax=630, fill="#ffffff") +

    # Left accent stripe
    annotate("rect", xmin=0, xmax=7, ymin=0, ymax=630, fill=state_col) +

    # Icon — square, centred in left panel
    annotation_raster(icon_img, xmin=20, xmax=420, ymin=100, ymax=570) +

    # Logo — bottom-centre of left panel
    {if (!is.null(logo_img))
      annotation_raster(logo_img, xmin=170, xmax=270, ymin=5, ymax=95)} +

    # ── Right panel ────────────────────────────────────────────────────────

    # Park name — top
    annotate("text", x=rx, y=592, label=park_name,
             color=text_main, size=9, fontface="bold", hjust=0) +

    # Date — larger, just below park name
    annotate("text", x=rx, y=545, label=date_txt,
             color=text_sub, size=7, hjust=0) +

    # Divider
    annotate("segment", x=rx, xend=1170, y=515, yend=515,
             color=text_sub, linewidth=0.4, alpha=0.5) +

    # "TODAY'S CONDITIONS" section label
    annotate("text", x=rx, y=492, label="TODAY\u2019S CONDITIONS",
             color=text_sub, size=4.5, fontface="bold", hjust=0) +

    # Condition label (coloured, large)
    annotate("text", x=rx, y=443, label=cond_txt,
             color=state_col, size=11, fontface="bold", hjust=0) +

    # Weather stats
    {if (!is.null(temp_txt))
      annotate("text", x=rx, y=385,
               label=paste0("\u26a1 High: ", temp_txt),
               color=text_main, size=6, hjust=0)} +
    {if (!is.null(precip_txt))
      annotate("text", x=rx, y=338,
               label=paste0("\U0001f327 ", precip_txt),
               color=text_main, size=6, hjust=0)} +

    # Divider
    annotate("segment", x=rx, xend=1170, y=305, yend=305,
             color=text_sub, linewidth=0.4, alpha=0.5) +

    # "3-DAY MUD FORECAST" section label
    annotate("text", x=rx, y=282, label="3-DAY MUD FORECAST",
             color=text_sub, size=4.5, fontface="bold", hjust=0) +

    # Forecast summary text
    {if (!is.null(fc_txt))
      annotate("text", x=rx, y=238, label=fc_txt,
               color=accent, size=7.5, fontface="bold", hjust=0)} +

    # TrailPulse brand — bottom
    annotate("text", x=rx, y=45, label="TrailPulse",
             color=accent, size=8.5, fontface="bold", hjust=0) +

    coord_cartesian(xlim=c(0, 1200), ylim=c(0, 630), expand=FALSE) +
    theme_void()

  dir.create(dirname(output_path), showWarnings=FALSE, recursive=TRUE)
  ggsave(output_path, p,
         width  = width_px  / 100,
         height = height_px / 100,
         dpi    = 100,
         bg     = bg_dark)

  invisible(output_path)
}
