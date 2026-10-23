# R/og_card.R
# Generates a 1200×630 social-media card (Open Graph / Twitter) for a park page.
# Visual: short-window TrailPulse score graph (yesterday → +2 days).
# Scale labels live INSIDE the plot on the left edge (no external scale bar),
# aligned to Avoid→Great bands; forecast segment is dashed with open points.
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

# ── Condition rank for an explicit mode ──────────────────────────────────────
.og_rank <- function(mode, condition_score) {
  score <- if (is.null(condition_score) || is.na(condition_score)) 5.0
           else as.numeric(condition_score)
  score <- pmax(1, pmin(10, score))
  lbl   <- score_to_label(score)
  if (is.na(lbl)) lbl <- "Poor"
  clr   <- condition_color(lbl)
  list(label = lbl, rank_col = clr, score = score)
}

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

# Point color from score — same breaks as condition_color / plots.R bands.
.score_pt_color <- function(score) {
  dplyr::case_when(
    is.na(score)       ~ "#888888",
    score >= 8         ~ unname(CONDITION_COLORS_5["Great"]),
    score >= 6         ~ unname(CONDITION_COLORS_5["Good"]),
    score >= 2         ~ unname(CONDITION_COLORS_5["Poor"]),
    TRUE               ~ unname(CONDITION_COLORS_5["Avoid"])
  )
}

# ── Short-window score series (yesterday → +2 days) ──────────────────────────
# Returns a tibble: day_index, date, score, is_forecast, x_label, pt_color.
short_window_score_series <- function(weather_df,
                                      mode = c("trail", "snow"),
                                      as_of = Sys.Date()) {
  mode  <- match.arg(mode)
  as_of <- as.Date(as_of)
  score_col <- if (identical(mode, "snow"))
    "snow_condition_score" else "condition_score"

  if (is.null(weather_df) || !score_col %in% names(weather_df)) {
    return(tibble::tibble(
      day_index   = 1:4,
      date        = as_of + (-1L:2L),
      score       = NA_real_,
      is_forecast = c(FALSE, FALSE, TRUE, TRUE),
      x_label     = c("Yesterday", "Today", "Tomorrow", "+2 days"),
      pt_color    = "#888888"
    ))
  }

  days   <- as_of + (-1L:2L)
  labels <- c("Yesterday", "Today", "Tomorrow", "+2 days")

  rows <- lapply(seq_along(days), function(i) {
    d   <- days[[i]]
    sub <- weather_df |> dplyr::filter(date == d)
    sc  <- NA_real_
    if (nrow(sub) > 0) {
      if (d <= as_of) {
        hist <- sub |>
          dplyr::filter(is.na(source) | source != "forecast")
        if (nrow(hist) > 0) sub <- hist
      } else {
        fc <- sub |> dplyr::filter(source == "forecast")
        if (nrow(fc) > 0) sub <- fc
      }
      sc <- suppressWarnings(as.numeric(sub[[score_col]][[1]]))
    }
    tibble::tibble(
      day_index   = i,
      date        = d,
      score       = sc,
      is_forecast = d > as_of,
      x_label     = labels[[i]]
    )
  })

  out <- dplyr::bind_rows(rows)

  # Fill gaps with nearest available score so the line still renders.
  if (all(is.na(out$score))) {
    out$score <- 5.0
  } else {
    for (i in seq_len(nrow(out))) {
      if (!is.na(out$score[[i]])) next
      known <- which(!is.na(out$score))
      j <- known[which.min(abs(known - i))]
      out$score[[i]] <- out$score[[j]]
    }
  }

  out$score    <- pmax(1, pmin(10, out$score))
  out$pt_color <- .score_pt_color(out$score)
  out
}

# ── Short-window ggplot (shared by OG card + park heroes) ────────────────────
# Band breaks match CONDITION_BREAKS / plots.R: Avoid <2, Poor <6, Good <8, Great.
# Labels sit INSIDE the panel on the left edge at each band midpoint.
# Observed (yesterday→today): solid line + filled points.
# Forecast (today→+2): dashed line + hollow points.
plot_short_window_score <- function(series,
                                    y_min = 0,
                                    y_max = 10,
                                    show_y_numbers = TRUE,
                                    base_size = 11) {
  stopifnot(is.data.frame(series), nrow(series) >= 2)
  series <- series |>
    dplyr::arrange(day_index) |>
    dplyr::mutate(
      x_label = factor(x_label, levels = x_label),
      x_num   = as.numeric(day_index)
    )

  # Segment tables: solid through today; dashed from today onward.
  today_i <- which(!series$is_forecast)
  today_i <- if (length(today_i)) max(today_i) else 2L
  obs_df  <- series |> dplyr::filter(day_index <= today_i)
  fc_df   <- series |> dplyr::filter(day_index >= today_i)

  # Band rects — same y breaks as condition_color / site score bands.
  bands <- tibble::tibble(
    ymin  = c(y_min, 2, 6, 8),
    ymax  = c(2, 6, 8, y_max),
    label = c("Avoid", "Poor", "Good", "Great"),
    fill  = unname(CONDITION_COLORS_5[c("Avoid", "Poor", "Good", "Great")])
  )
  bands$y_mid <- (bands$ymin + bands$ymax) / 2

  x_min <- 0.55
  x_max <- max(series$x_num) + 0.45
  # Leave a left gutter inside the panel for scale labels.
  label_x <- x_min + 0.08

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(xmin = x_min, xmax = x_max, ymin = ymin, ymax = ymax, fill = fill),
      alpha = 0.18, color = NA, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    # In-panel scale labels (left edge), colored to match bands — no external bar.
    ggplot2::geom_text(
      data = bands,
      ggplot2::aes(x = label_x, y = y_mid, label = label, color = fill),
      hjust = 0, vjust = 0.5, size = base_size * 0.32, fontface = "bold",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_color_identity() +
    # Today vertical guide + "Forecast →"
    ggplot2::geom_vline(
      xintercept = today_i, linetype = "dashed",
      color = "#9ca3af", linewidth = 0.45
    ) +
    ggplot2::annotate(
      "text",
      x = today_i + 0.12, y = y_max - 0.35,
      label = "Forecast \u2192",
      hjust = 0, vjust = 1, size = base_size * 0.28, color = "#6b7280"
    ) +
    # Observed solid
    { if (nrow(obs_df) >= 2)
        ggplot2::geom_line(
          data = obs_df,
          ggplot2::aes(x = x_num, y = score),
          color = "#1f2937", linewidth = 1.35, lineend = "round"
        ) } +
    # Forecast dashed
    { if (nrow(fc_df) >= 2)
        ggplot2::geom_line(
          data = fc_df,
          ggplot2::aes(x = x_num, y = score),
          color = "#1f2937", linewidth = 1.25, linetype = "22",
          lineend = "round"
        ) } +
    # Observed points (filled)
    ggplot2::geom_point(
      data = series |> dplyr::filter(!is_forecast),
      ggplot2::aes(x = x_num, y = score),
      color = (series |> dplyr::filter(!is_forecast))$pt_color,
      size = 3.4, stroke = 0
    ) +
    # Forecast points (hollow / open)
    ggplot2::geom_point(
      data = series |> dplyr::filter(is_forecast),
      ggplot2::aes(x = x_num, y = score),
      shape = 21,
      fill = "white",
      color = (series |> dplyr::filter(is_forecast))$pt_color,
      size = 3.4, stroke = 1.4
    ) +
    ggplot2::scale_x_continuous(
      breaks = series$x_num,
      labels = series$x_label,
      limits = c(x_min, x_max),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = if (isTRUE(show_y_numbers)) c(2, 4, 6, 8, 10) else NULL,
      position = "right",
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e5e7eb", linewidth = 0.3),
      panel.background   = ggplot2::element_rect(fill = "white", color = NA),
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      axis.title         = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        color = "#374151",
        size = base_size * 0.85
      ),
      axis.text.y = ggplot2::element_text(color = "#6b7280", size = base_size * 0.75),
      plot.margin = ggplot2::margin(8, 10, 6, 6)
    )

  p
}

# Save a standalone short-window PNG (park hero / landing reuse).
save_short_window_score <- function(series,
                                    path,
                                    width_in = 7.2,
                                    height_in = 2.9,
                                    dpi = 140) {
  p <- plot_short_window_score(series, base_size = 12)
  out_dir <- dirname(path)
  if (!identical(out_dir, ".") && !identical(out_dir, "")) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  ggplot2::ggsave(path, p, width = width_in, height = height_in, dpi = dpi, bg = "white")
  invisible(path)
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
# Layout: brand + park identity → status chip → short-window graph (hero) →
# compact secondary stats. No cartoon faces. No GO/NO-GO.

generate_og_card <- function(
    park_name            = "Hunters Creek Park",
    location             = "Wales, NY",
    mode                 = c("trail", "snow"),
    condition_score      = NULL,
    score_series         = NULL,  # from short_window_score_series(); optional
    temp_high_f          = NULL,
    temp_low_f           = NULL,
    precip_7d_mm         = NULL,
    forecast_summary     = NULL,
    snow_depth_cm        = NULL,
    snowfall_48h_cm      = NULL,
    show_face            = FALSE, # retained for API compat; faces no longer drawn
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

  # Prefer today's score from the series when available.
  if (!is.null(score_series) && nrow(score_series) > 0) {
    today_row <- score_series[!score_series$is_forecast, , drop = FALSE]
    if (nrow(today_row) > 0 && (is.null(condition_score) || is.na(condition_score))) {
      condition_score <- today_row$score[[nrow(today_row)]]
    }
  }

  rank  <- .og_rank(mode, condition_score)
  lbl   <- rank$label
  score <- rank$score

  if (is.null(score_series) || nrow(score_series) == 0) {
    # Flat fallback so the card still renders without weather rows.
    score_series <- tibble::tibble(
      day_index   = 1:4,
      date        = Sys.Date() + (-1L:2L),
      score       = score,
      is_forecast = c(FALSE, FALSE, TRUE, TRUE),
      x_label     = c("Yesterday", "Today", "Tomorrow", "+2 days"),
      pt_color    = .score_pt_color(score)
    )
  }

  lp       <- .logo_path()
  logo_img <- if (!is.null(lp)) png::readPNG(lp) else NULL

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

  date_txt   <- format(Sys.Date(), "%B %d, %Y")
  score_txt  <- sprintf("%.1f", score)
  mode_label <- if (identical(mode, "snow")) "SNOW" else "TRAIL"

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

  # Render graph to a temp PNG, then place on the OG canvas (no external scale bar).
  tmp_graph <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_graph), add = TRUE)
  ggplot2::ggsave(
    tmp_graph,
    plot_short_window_score(score_series, base_size = 14),
    width = 10.4, height = 4.05, dpi = 100, bg = "white"
  )
  graph_img <- png::readPNG(tmp_graph)

  # Geometry (y grows upward in annotate space)
  mx     <- 56
  mx_end <- 1144
  # Graph panel
  gx0 <- 48;  gx1 <- 1152
  gy0 <- 88;  gy1 <- 430

  p <- ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1200, ymin = 0, ymax = 630,
                      fill = bg_card) +
    ggplot2::annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 630,
                      fill = accent) +

    # Brand + park identity (left)
    { if (!is.null(logo_img))
        ggplot2::annotation_raster(logo_img, xmin = mx, xmax = mx + 40,
                                   ymin = 568, ymax = 608) } +
    ggplot2::annotate(
      "text",
      x = if (!is.null(logo_img)) mx + 48 else mx,
      y = 588, label = "TrailPulse",
      color = accent, size = 5.5, fontface = "bold", hjust = 0, vjust = 0.5
    ) +
    ggplot2::annotate("text", x = mx, y = 545, label = park_name,
                      color = text_main, size = 8.2, fontface = "bold",
                      hjust = 0, vjust = 0.5) +
    ggplot2::annotate(
      "text", x = mx, y = 508,
      label = paste0(location, "  \u00b7  Short-window score"),
      color = text_sub, size = 4.6, hjust = 0, vjust = 0.5
    ) +

    # Mode chip + status (right)
    ggplot2::annotate("rect", xmin = 900, xmax = 980, ymin = 572, ymax = 604,
                      fill = text_main, color = NA) +
    ggplot2::annotate("text", x = 940, y = 588, label = mode_label,
                      color = "white", size = 3.6, fontface = "bold",
                      hjust = 0.5, vjust = 0.5) +
    ggplot2::annotate("text", x = mx_end, y = 545, label = lbl,
                      color = accent, size = 9, fontface = "bold",
                      hjust = 1, vjust = 0.5) +
    ggplot2::annotate(
      "text", x = mx_end, y = 508,
      label = paste0(score_txt, " / 10"),
      color = accent, size = 5.8, fontface = "bold",
      hjust = 1, vjust = 0.5
    ) +

    # White graph panel + short-window chart (labels already inside the plot)
    ggplot2::annotate("rect", xmin = gx0 - 4, xmax = gx1 + 4,
                      ymin = gy0 - 4, ymax = gy1 + 4,
                      fill = "white", color = "#d1d5db", linewidth = 0.4) +
    ggplot2::annotation_raster(graph_img, xmin = gx0, xmax = gx1,
                               ymin = gy0, ymax = gy1) +

    # Compact secondary stats under the graph
    { if (!is.null(stat1))
        ggplot2::annotate("text", x = mx, y = 58, label = stat1,
                          color = text_main, size = 4.8, hjust = 0) } +
    { if (!is.null(stat2))
        ggplot2::annotate("text", x = mx_end, y = 58, label = stat2,
                          color = text_main, size = 4.6, hjust = 1) } +
    # Subtle as-of date
    ggplot2::annotate(
      "text", x = mx, y = 28, label = date_txt,
      color = text_sub, size = 3.6, hjust = 0
    ) +

    ggplot2::coord_cartesian(xlim = c(0, 1200), ylim = c(0, 630), expand = FALSE) +
    ggplot2::theme_void()

  out_dir <- dirname(output_path)
  if (!identical(out_dir, ".") && !identical(out_dir, "")) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  ggplot2::ggsave(output_path, p,
                  width  = width_px  / 100,
                  height = height_px / 100,
                  dpi    = 100,
                  bg     = bg_card)

  invisible(list(
    path         = output_path,
    mode         = mode,
    label        = lbl,
    score        = score,
    title        = meta$title,
    description  = meta$description,
    score_series = score_series
  ))
}
