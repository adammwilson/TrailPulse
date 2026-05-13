# R/plots.R
# Shared plotting functions for TrailPulse.
# All dygraph functions return dygraphs objects.
# All ggplot functions return ggplot objects.
#
# Depends on: setup.R

# ── Color for mud level name ─────────────────────────────────────────────────
.mud_color <- function(level_name) {
  MUD_COLORS[level_name] %||% "#888"
}

# ── Shared dygraph styling helper ────────────────────────────────────────────
.dy_style <- function(dg, title = NULL) {
  dg <- dg |>
    dyOptions(
      drawGrid        = TRUE,
      gridLineColor   = "#e0e0e0",
      axisLineColor   = "#999",
      axisLabelColor  = "#555",
      fillAlpha       = 0.15,
      strokeWidth     = 2
    ) |>
    dyRangeSelector(height = 24, strokeColor = "") |>
    dyCSS(textConnection("
      .dygraph-legend { font-size: 12px; }
    "))

  if (!is.null(title)) dg <- dyOptions(dg, title = title)
  dg
}

# ────────────────────────────────────────────────────────────────────────────
# 1.  Mud forecast: 14-day history + 7-day forecast bar chart (ggplot2)
# ────────────────────────────────────────────────────────────────────────────
plot_mud_forecast <- function(weather_mud_df, history_days = HISTORY_DAYS) {
  today  <- Sys.Date()
  window <- weather_mud_df |>
    filter(date >= today - history_days, date <= today + FORECAST_DAYS) |>
    mutate(
      bar_alpha  = if_else(source == "forecast", 0.55, 0.9),
      bar_fill   = coalesce(MUD_COLORS[mud_level_name], "#aaa"),
      lbl_date   = format(date, "%b %d")
    )

  ggplot(window, aes(x = date, y = pmax(mud_level, 0.3), fill = mud_level_name)) +
    geom_col(aes(alpha = bar_alpha), width = 0.85, color = NA) +
    geom_vline(xintercept = as.numeric(today) - 0.5, linetype = "dashed",
               color = "#555", linewidth = 0.7) +
    annotate("text", x = today - 0.4, y = 9.5, label = "Today",
             hjust = 1, size = 3, color = "#555") +
    scale_fill_manual(values = MUD_COLORS, drop = FALSE, name = NULL) +
    scale_alpha_identity() +
    scale_y_continuous(
      limits = c(0, 10),
      breaks = seq(0, 10, 2),
      labels = c("Bone Dry", "Dusty", "Firm", "Soft", "Very Muddy", "Impassable")
    ) +
    scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
    labs(
      x = NULL, y = NULL,
      caption = "Hatched bars = forecast. Dashed line = today."
    ) +
    theme_trailpulse() +
    theme(
      legend.position   = "none",
      axis.text.x       = element_text(angle = 30, hjust = 1, size = 8),
      panel.grid.major.x = element_blank()
    )
}

# ────────────────────────────────────────────────────────────────────────────
# 2. Snow forecast: 14-day history + 7-day forecast (ggplot2)
#    Bars = daily snowfall; line = snow depth
# ────────────────────────────────────────────────────────────────────────────
plot_snow_forecast <- function(weather_df, history_days = HISTORY_DAYS) {
  today  <- Sys.Date()
  window <- weather_df |>
    filter(date >= today - history_days, date <= today + FORECAST_DAYS) |>
    mutate(bar_alpha = if_else(source == "forecast", 0.5, 0.85))

  # Dual-axis: snowfall (bars, left) and snow depth (line, right)
  depth_scale <- max(window$snow_depth, na.rm = TRUE)
  depth_scale <- if (is.finite(depth_scale) && depth_scale > 0) depth_scale else 30
  snow_scale  <- max(window$snowfall, na.rm = TRUE)
  snow_scale  <- if (is.finite(snow_scale) && snow_scale > 0) snow_scale else 5

  ggplot(window, aes(x = date)) +
    geom_col(aes(y = snowfall, alpha = bar_alpha),
             fill = "#64b5f6", width = 0.8, color = NA) +
    geom_line(aes(y = snow_depth * snow_scale / depth_scale),
              color = "#1e88e5", linewidth = 1.2, na.rm = TRUE) +
    geom_point(aes(y = snow_depth * snow_scale / depth_scale),
               color = "#1e88e5", size = 1.5, na.rm = TRUE) +
    geom_vline(xintercept = as.numeric(today) - 0.5, linetype = "dashed",
               color = "#555", linewidth = 0.7) +
    annotate("text", x = today - 0.4, y = snow_scale * 0.95, label = "Today",
             hjust = 1, size = 3, color = "#555") +
    scale_alpha_identity() +
    scale_y_continuous(
      name      = "Snowfall (cm)",
      sec.axis  = sec_axis(~ . * depth_scale / snow_scale, name = "Snow Depth (cm)")
    ) +
    scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
    labs(
      x = NULL,
      caption = "Bars = daily snowfall (light = forecast). Line = snow depth on ground."
    ) +
    theme_trailpulse() +
    theme(
      axis.text.x        = element_text(angle = 30, hjust = 1, size = 8),
      panel.grid.major.x = element_blank()
    )
}

# ────────────────────────────────────────────────────────────────────────────
# 3. Cumulative precip YTD spaghetti (dygraph)
#    All years: grey lines. Current year: bold color.
# ────────────────────────────────────────────────────────────────────────────
plot_cumulative_precip_ytd <- function(weather_df) {
  today_year <- year(Sys.Date())

  cumprec <- weather_df |>
    filter(!is.na(precip), source == "history" | date <= Sys.Date()) |>
    mutate(cal_year = year(date), doy = yday(date)) |>
    group_by(cal_year) |>
    arrange(date) |>
    mutate(cum_precip = cumsum(coalesce(precip, 0))) |>
    ungroup() |>
    select(cal_year, doy, cum_precip)

  # Pivot to wide: one column per year
  wide <- cumprec |>
    pivot_wider(names_from = cal_year, values_from = cum_precip) |>
    arrange(doy)

  mat  <- as.matrix(select(wide, -doy))
  xts_obj <- xts::xts(
    mat,
    order.by = as.Date(paste0("2000-", wide$doy), format = "%Y-%j")
  )

  yr_cols  <- setdiff(colnames(xts_obj), as.character(today_year))
  cur_col  <- as.character(today_year)

  dg <- dygraph(xts_obj, main = "Cumulative Precipitation by Year") |>
    dyAxis("x", label = "Day of Year") |>
    dyAxis("y", label = "Cumulative Precipitation (mm)")

  for (yr in yr_cols) {
    dg <- dySeries(dg, yr, color = "#cccccc", strokeWidth = 1, drawPoints = FALSE)
  }

  if (cur_col %in% colnames(xts_obj)) {
    dg <- dySeries(dg, cur_col, color = "#d44000", strokeWidth = 2.5, drawPoints = FALSE)
  }

  dg |>
    dyLegend(show = "onmouseover", hideOnMouseOut = TRUE) |>
    .dy_style()
}

# ────────────────────────────────────────────────────────────────────────────
# 4. Soil moisture history vs. percentile envelope (dygraph)
# ────────────────────────────────────────────────────────────────────────────
plot_soil_moisture_history <- function(weather_df, recent_years = 5) {
  today <- Sys.Date()

  sm <- weather_df |>
    filter(!is.na(soil_moisture_0_1), source == "history" | date <= today) |>
    mutate(
      soil_wetness = 0.50 * soil_moisture_0_1 +
                     0.35 * coalesce(soil_moisture_1_3, soil_moisture_0_1) +
                     0.15 * coalesce(soil_moisture_3_9, soil_moisture_0_1),
      doy = yday(date)
    )

  # Historical percentile envelope (excluding most recent years)
  hist_env <- sm |>
    filter(year(date) < year(today) - recent_years) |>
    group_by(doy) |>
    summarise(
      p10 = quantile(soil_wetness, 0.10, na.rm = TRUE),
      p25 = quantile(soil_wetness, 0.25, na.rm = TRUE),
      p75 = quantile(soil_wetness, 0.75, na.rm = TRUE),
      p90 = quantile(soil_wetness, 0.90, na.rm = TRUE),
      .groups = "drop"
    )

  recent <- sm |>
    filter(year(date) >= year(today) - recent_years) |>
    select(date, soil_wetness)

  recent_xts <- xts::xts(recent$soil_wetness, order.by = recent$date)
  colnames(recent_xts) <- "Soil Wetness"

  dygraph(recent_xts, main = "Soil Wetness (top 9 cm)") |>
    dyAxis("y", label = "Soil Wetness (m³/m³)", valueRange = c(0, 0.65)) |>
    dySeries("Soil Wetness", color = "#6B3B18", strokeWidth = 2) |>
    dyShading(from = 0, to = 0.2,  color = "#f5e8d8") |>
    dyShading(from = 0.4, to = 0.65, color = "#c0d8f0") |>
    dyLegend(show = "onmouseover") |>
    .dy_style()
}

# ────────────────────────────────────────────────────────────────────────────
# 5. Streamflow year-over-year (dygraph)
# ────────────────────────────────────────────────────────────────────────────
plot_streamflow <- function(stream_df) {
  today_wy <- if (month(Sys.Date()) >= 10) year(Sys.Date()) else year(Sys.Date()) - 1L

  wide <- stream_df |>
    filter(!is.na(roll7_cfs)) |>
    select(water_year, dowy, roll7_cfs) |>
    pivot_wider(names_from = water_year, values_from = roll7_cfs) |>
    arrange(dowy)

  # Map dowy → fake date for dygraph x-axis
  xts_obj <- xts::xts(
    as.matrix(select(wide, -dowy)),
    order.by = as.Date("2000-10-01") + wide$dowy - 1L
  )

  yr_cols <- setdiff(colnames(xts_obj), as.character(today_wy))
  cur_col <- as.character(today_wy)

  dg <- dygraph(xts_obj, main = "Streamflow (7-day rolling median)") |>
    dyAxis("x", label = "Month") |>
    dyAxis("y", label = "Discharge (cfs)", logscale = TRUE)

  for (yr in yr_cols) {
    dg <- dySeries(dg, yr, color = "#cccccc", strokeWidth = 1)
  }

  if (cur_col %in% colnames(xts_obj)) {
    dg <- dySeries(dg, cur_col, color = "#1565C0", strokeWidth = 2.5)
  }

  dg |>
    dyLegend(show = "onmouseover", hideOnMouseOut = TRUE) |>
    .dy_style()
}

# ────────────────────────────────────────────────────────────────────────────
# 6. Cumulative seasonal snowfall spaghetti (dygraph)
# ────────────────────────────────────────────────────────────────────────────
plot_cumulative_snowfall <- function(weather_df) {
  today_sy <- if (month(Sys.Date()) >= 11) year(Sys.Date()) else year(Sys.Date()) - 1L

  cum_sf <- compute_cumulative_snowfall(weather_df)

  wide <- cum_sf |>
    select(snow_year, doss, cum_snowfall_cm) |>
    pivot_wider(names_from = snow_year, values_from = cum_snowfall_cm) |>
    arrange(doss)

  xts_obj <- xts::xts(
    as.matrix(select(wide, -doss)),
    order.by = as.Date("2000-11-01") + wide$doss - 1L
  )

  yr_cols <- setdiff(colnames(xts_obj), as.character(today_sy))
  cur_col <- as.character(today_sy)

  dg <- dygraph(xts_obj, main = "Cumulative Snowfall by Season") |>
    dyAxis("x", label = "Day of Snow Season (Nov 1 = 0)") |>
    dyAxis("y", label = "Cumulative Snowfall (cm)")

  for (yr in yr_cols) {
    dg <- dySeries(dg, yr, color = "#cccccc", strokeWidth = 1)
  }

  if (cur_col %in% colnames(xts_obj)) {
    dg <- dySeries(dg, cur_col, color = "#1e88e5", strokeWidth = 2.5)
  }

  dg |>
    dyLegend(show = "onmouseover", hideOnMouseOut = TRUE) |>
    .dy_style()
}

# ────────────────────────────────────────────────────────────────────────────
# 7. Peak snow depth per winter – dot plot (ggplot2)
# ────────────────────────────────────────────────────────────────────────────
plot_snow_depth_history <- function(weather_df) {
  today_sy <- if (month(Sys.Date()) >= 11) year(Sys.Date()) else year(Sys.Date()) - 1L
  peak     <- compute_peak_snow_depth(weather_df) |>
    filter(snow_year >= year(WEATHER_START))

  ggplot(peak, aes(x = snow_year, y = peak_depth_cm)) +
    geom_col(
      aes(fill = snow_year == today_sy),
      width = 0.7,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("FALSE" = "#b0d4ec", "TRUE" = "#1e88e5")) +
    geom_text(
      data = filter(peak, snow_year == today_sy),
      aes(label = glue("{round(peak_depth_cm)} cm")),
      vjust = -0.5, size = 3.5, color = "#1e88e5", fontface = "bold"
    ) +
    labs(
      x = "Snow Season (year starting Nov)",
      y = "Peak Snow Depth (cm)",
      title = "Peak Snow Depth by Winter"
    ) +
    theme_trailpulse() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
}

# ────────────────────────────────────────────────────────────────────────────
# 8. Mini mudface strip: 7 small faces for forecast days (htmltools)
# ────────────────────────────────────────────────────────────────────────────
mud_forecast_strip <- function(weather_mud_df) {
  forecast_rows <- weather_mud_df |>
    filter(source == "forecast", date >= Sys.Date()) |>
    arrange(date) |>
    slice_head(n = 7)

  if (nrow(forecast_rows) == 0) return(htmltools::HTML(""))

  day_divs <- purrr::pmap(forecast_rows, function(date, mud_level, mud_level_name, ...) {
    face_svg <- make_mudface(mud_level, size = "70px", label = FALSE)
    htmltools::tags$div(
      class = "tp-forecast-day",
      htmltools::tags$span(class = "day-label", format(date, "%a")),
      face_svg,
      htmltools::tags$span(
        class = "mud-label",
        if (is.na(mud_level_name)) "?" else mud_level_name
      )
    )
  })

  htmltools::div(class = "tp-forecast-strip", !!!day_divs)
}

# ────────────────────────────────────────────────────────────────────────────
# 9. Mini snowface strip: 7 small skiers for forecast days (htmltools)
# ────────────────────────────────────────────────────────────────────────────
snow_forecast_strip <- function(weather_df) {
  forecast_rows <- weather_df |>
    filter(source == "forecast", date >= Sys.Date()) |>
    arrange(date) |>
    slice_head(n = 7)

  if (nrow(forecast_rows) == 0) return(htmltools::HTML(""))

  day_divs <- purrr::pmap(forecast_rows, function(date, snow_depth, ...) {
    face_svg <- make_snowface(coalesce(snow_depth, 0), size = "70px", label = FALSE)
    htmltools::tags$div(
      class = "tp-forecast-day",
      htmltools::tags$span(class = "day-label", format(date, "%a")),
      face_svg,
      htmltools::tags$span(
        class = "mud-label",
        glue("{round(coalesce(snow_depth, 0))} cm")
      )
    )
  })

  htmltools::div(class = "tp-forecast-strip", !!!day_divs)
}

# ────────────────────────────────────────────────────────────────────────────
# 10. NWS forecast card strip (htmltools)
# ────────────────────────────────────────────────────────────────────────────
nws_forecast_strip <- function(nws_df) {
  if (nrow(nws_df) == 0) return(htmltools::HTML(""))

  day_rows <- nws_df |> filter(is_daytime) |> slice_head(n = 7)

  cards <- purrr::pmap(day_rows, function(period_name, temp_c, precip_pct,
                                          short_forecast, icon_url, ...) {
    htmltools::tags$div(
      class = "tp-forecast-day",
      style = "min-width:80px",
      htmltools::tags$span(class = "day-label", period_name),
      if (!is.na(icon_url))
        htmltools::tags$img(src = icon_url, width = "50px", height = "50px",
                            alt = short_forecast),
      htmltools::tags$div(
        style = "font-size:0.8rem",
        glue("{round(temp_c)}°C"),
        if (!is.na(precip_pct)) glue(" | {round(precip_pct)}% precip") else ""
      ),
      htmltools::tags$div(
        style = "font-size:0.7rem; color:#666; max-width:80px; text-align:center",
        short_forecast
      )
    )
  })

  htmltools::div(
    class = "tp-forecast-strip",
    style = "flex-wrap:wrap; gap:0.75rem",
    !!!cards
  )
}
