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
.dy_style <- function(dg, title = NULL, range_selector = TRUE) {
  x_lbl <- paste0(
    "function(d,gran){",
    "var m=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];",
    "return m[d.getMonth()]+' '+d.getDate();}"
  )
  dg <- dg |>
    dyOptions(
      drawGrid        = TRUE,
      gridLineColor   = "#e0e0e0",
      axisLineColor   = "#999",
      axisLabelColor  = "#555",
      fillAlpha       = 0.15,
      strokeWidth     = 2
    ) |>
    dyAxis("x", axisLabelFormatter = x_lbl) |>
    dyCSS(textConnection("
      .dygraph-legend { font-size: 12px; }
    "))

  if (range_selector) dg <- dg |> dyRangeSelector(height = 24, strokeColor = "")
  if (!is.null(title)) dg <- dyOptions(dg, title = title)
  dg
}

# ────────────────────────────────────────────────────────────────────────────
# 1.  Current conditions + forecast — 3 compact shared-axis panels (patchwork)
#     (a) Temperature + Humidity  (b) Soil Moisture 0-9cm + Precip
#     (c) Streamflow daily discharge (optional)
#     X-axis labels on bottom panel only. No spaghetti — ribbons only.
# ────────────────────────────────────────────────────────────────────────────
plot_current_conditions <- function(weather_mud_df, stream_df = NULL) {
  today        <- Sys.Date()
  window_start <- today - 14L
  window_end   <- today + FORECAST_DAYS
  today_year   <- year(today)

  wx      <- weather_mud_df |> filter(date >= window_start, date <= window_end)
  hist_wx <- weather_mud_df |> filter(source == "history", year(date) < today_year)

  all_dates <- seq(window_start, window_end, by = "day")
  win_dates <- tibble(date = all_dates, doy = yday(all_dates))

  .map_clim <- function(doy_df) {
    win_dates |> left_join(doy_df, by = "doy") |> arrange(date)
  }

  # 31-day rolling mean smooths climatological DOY quantiles
  .rollm <- function(x) zoo::rollapply(x, width = 31L, FUN = mean, na.rm = TRUE,
                                       fill = NA, partial = TRUE, align = "center")

  # ── Shared annotation layers ──────────────────────────────────────────────
  fc_shade <- annotate("rect",
                       xmin = today, xmax = window_end + 1L,
                       ymin = -Inf, ymax = Inf, fill = "#f0f0f0", alpha = 0.55)
  today_vl <- geom_vline(xintercept = today,
                         color = "#888888", linewidth = 0.4, linetype = "dashed")

  # Day-name x-axis: "Today" for today, abbreviated weekday names for others
  day_lbl  <- function(d) { lbl <- weekdays(d, abbreviate = TRUE); lbl[d == today] <- "Today"; lbl }
  breaks_x <- sort(unique(c(today, all_dates[seq(1, length(all_dates), by = 2)])))
  x_sc <- scale_x_date(labels = day_lbl, breaks = breaks_x,
                       minor_breaks = all_dates, expand = expansion(add = 0.5))

  # Theme for upper panels: hide x-axis (y-axis title colors set per-panel)
  th_upper <- theme(
    axis.title.x       = element_blank(),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y        = element_text(size = 10),
    plot.margin        = margin(2, 6, 0, 2)
  )
  # Bottom panel gets x-axis labels
  th_bottom <- th_upper + theme(
    axis.text.x  = element_text(size = 10, angle = 30, hjust = 1),
    axis.ticks.x = element_line(),
    plot.margin  = margin(0, 6, 2, 2)
  )

  # ── (a) Temperature (left °C) + Humidity (right %) ───────────────────────
  temp_clim <- hist_wx |>
    filter(!is.na(temp_max), !is.na(temp_min)) |>
    mutate(doy = yday(date), temp_mean = (temp_min + temp_max) / 2) |>
    group_by(doy) |>
    summarise(clim_lo  = quantile(temp_min,  0.10, na.rm = TRUE),
              clim_mid = quantile(temp_mean, 0.50, na.rm = TRUE),
              clim_hi  = quantile(temp_max,  0.90, na.rm = TRUE), .groups = "drop") |>
    arrange(doy) |> mutate(across(starts_with("clim"), .rollm)) |> .map_clim()

  humid_clim <- hist_wx |>
    filter(!is.na(humidity_max), !is.na(humidity_min)) |>
    mutate(doy = yday(date), hum_mean = (humidity_min + humidity_max) / 2) |>
    group_by(doy) |>
    summarise(clim_lo  = quantile(humidity_min, 0.10, na.rm = TRUE),
              clim_mid = quantile(hum_mean,     0.50, na.rm = TRUE),
              clim_hi  = quantile(humidity_max, 0.90, na.rm = TRUE), .groups = "drop") |>
    arrange(doy) |> mutate(across(starts_with("clim"), .rollm)) |> .map_clim()

  temp_obs <- wx |>
    filter(!is.na(temp_max) | !is.na(temp_min)) |>
    mutate(temp_mean = (coalesce(temp_min, temp_max) + coalesce(temp_max, temp_min)) / 2)

  humid_obs <- wx |>
    filter(!is.na(humidity_max) | !is.na(humidity_min)) |>
    mutate(hum_mean = (coalesce(humidity_min, humidity_max) +
                       coalesce(humidity_max, humidity_min)) / 2)

  # Convert temp to °F for display
  c2f       <- function(c) c * 9/5 + 32
  temp_clim <- temp_clim |> mutate(across(c(clim_lo, clim_mid, clim_hi), c2f))
  temp_obs  <- temp_obs  |> mutate(temp_min  = c2f(temp_min),
                                    temp_max  = c2f(temp_max),
                                    temp_mean = c2f(temp_mean))

  # Map humidity 0-100% onto the °F temperature axis range
  t_lo <- floor(min(temp_clim$clim_lo,  temp_obs$temp_min, na.rm = TRUE)) - 3
  t_hi <- ceiling(max(temp_clim$clim_hi, temp_obs$temp_max, na.rm = TRUE)) + 3
  if (!is.finite(t_lo)) t_lo <- 14   # ~-10°C
  if (!is.finite(t_hi)) t_hi <- 95   # ~35°C
  h2f <- function(h) t_lo + h * (t_hi - t_lo) / 100
  f2h <- function(f) (f - t_lo) * 100 / (t_hi - t_lo)

  p_temp <- ggplot() +
    fc_shade + today_vl +
    geom_ribbon(data = temp_clim,  aes(date, ymin = clim_lo,  ymax = clim_hi),
                fill = "#e07030", alpha = 0.20) +
    geom_ribbon(data = humid_clim, aes(date, ymin = h2f(clim_lo), ymax = h2f(clim_hi)),
                fill = "#0077aa", alpha = 0.18) +
    geom_ribbon(data = temp_obs,   aes(date, ymin = temp_min,  ymax = temp_max),
                fill = "#e07030", alpha = 0.30) +
    geom_line(data   = temp_obs,   aes(date, temp_mean),
              color = "#c04010", linewidth = 1.2) +
    geom_ribbon(data = humid_obs,
                aes(date, ymin = h2f(humidity_min), ymax = h2f(humidity_max)),
                fill = "#0077aa", alpha = 0.25) +
    geom_line(data   = humid_obs,  aes(date, h2f(hum_mean)),
              color = "#005588", linewidth = 0.9) +
    scale_y_continuous(
      name     = "Temperature (\u00b0F)",
      limits   = c(t_lo, t_hi),
      sec.axis = sec_axis(f2h, name = "Humidity (%)",
                          labels = function(x) paste0(round(x), "%"))
    ) +
    x_sc + th_upper +
    theme(axis.title.y.left  = element_text(size = 10, face = "bold", color = "#c04010",
                                             margin = margin(r = 4)),
          axis.title.y.right = element_text(size = 10, face = "bold", color = "#005588"))

  # ── (b) Soil Moisture 0-9 cm (left) + Precipitation bars (right) ─────────
  .sm9 <- function(df) df |>
    mutate(sm9 = 0.50 * coalesce(soil_moisture_0_1, NA_real_) +
                 0.35 * coalesce(soil_moisture_1_3, soil_moisture_0_1) +
                 0.15 * coalesce(soil_moisture_3_9, soil_moisture_0_1))

  sm_obs  <- .sm9(wx)      |> filter(!is.na(sm9))
  sm_hist <- .sm9(hist_wx) |> filter(!is.na(sm9))

  sm_clim <- sm_hist |>
    mutate(doy = yday(date)) |>
    group_by(doy) |>
    summarise(clim_lo  = quantile(sm9, 0.10, na.rm = TRUE),
              clim_mid = quantile(sm9, 0.50, na.rm = TRUE),
              clim_hi  = quantile(sm9, 0.90, na.rm = TRUE), .groups = "drop") |>
    arrange(doy) |> mutate(across(starts_with("clim"), .rollm)) |> .map_clim()

  precip_obs <- wx |> filter(!is.na(precip))

  sm_top   <- max(sm_clim$clim_hi, sm_obs$sm9, na.rm = TRUE) * 1.15
  sm_top   <- if (is.finite(sm_top) && sm_top > 0) sm_top else 0.5
  prec_top <- max(precip_obs$precip, na.rm = TRUE)
  prec_top <- if (is.finite(prec_top) && prec_top > 0) prec_top else 20
  p2sm <- function(p) p * sm_top / prec_top
  sm2p <- function(s) s * prec_top / sm_top

  p_sm <- ggplot() +
    fc_shade + today_vl +
    geom_ribbon(data = sm_clim,    aes(date, ymin = clim_lo, ymax = clim_hi),
                fill = "#c8a47a", alpha = 0.45) +
    geom_line(data   = sm_clim,    aes(date, clim_mid),
              color = "#b08860", linewidth = 0.5, linetype = "dotted") +
    geom_col(data    = precip_obs, aes(date, p2sm(precip)),
             fill = "#1565C0", alpha = 0.60, width = 0.85) +
    geom_line(data   = sm_obs,     aes(date, sm9),
              color = "#6B3B18", linewidth = 1.3) +
    scale_y_continuous(
      name     = "Soil moisture (m\u00b3/m\u00b3)",
      limits   = c(0, sm_top),
      sec.axis = sec_axis(sm2p, name = "Precip (mm)")
    ) +
    x_sc + th_upper +
    theme(axis.title.y.left  = element_text(size = 10, face = "bold", color = "#6B3B18",
                                             margin = margin(r = 4)),
          axis.title.y.right = element_text(size = 10, face = "bold", color = "#1565C0"))

  panels <- list(p_sm, p_temp)

  # ── (c) Streamflow — daily discharge (optional) ───────────────────────────
  if (!is.null(stream_df) && nrow(stream_df) > 0) {
    sf_hist <- stream_df |>
      filter(year(date) < today_year, !is.na(discharge_cfs), discharge_cfs > 0)

    sf_clim <- sf_hist |>
      mutate(doy = yday(date)) |>
      group_by(doy) |>
      summarise(clim_lo  = quantile(discharge_cfs, 0.10, na.rm = TRUE),
                clim_mid = quantile(discharge_cfs, 0.50, na.rm = TRUE),
                clim_hi  = quantile(discharge_cfs, 0.90, na.rm = TRUE), .groups = "drop") |>
      arrange(doy) |> mutate(across(starts_with("clim"), .rollm)) |> .map_clim()

    sf_win <- stream_df |>
      filter(date >= window_start, date <= window_end,
             !is.na(discharge_cfs), discharge_cfs > 0)

    p_sf <- ggplot() +
      fc_shade + today_vl +
      geom_ribbon(data = sf_clim |> filter(!is.na(clim_lo), clim_lo > 0),
                  aes(date, ymin = clim_lo, ymax = clim_hi),
                  fill = "#90c4e4", alpha = 0.40) +
      geom_line(data = sf_clim |> filter(!is.na(clim_mid)),
                aes(date, clim_mid),
                color = "#70b4d4", linewidth = 0.5, linetype = "dotted") +
      geom_line(data = sf_win, aes(date, discharge_cfs),
                color = "#1565C0", linewidth = 1.2) +
      x_sc + th_upper +
      scale_y_log10(name = "Streamflow (cfs)", labels = scales::label_comma()) +
      theme(axis.title.y.left = element_text(size = 10, face = "bold", color = "#1565C0",
                                              margin = margin(r = 4)))

    panels <- c(panels, list(p_sf))
  }

  # Apply x-axis labels only to the bottom panel
  n           <- length(panels)
  panels[[n]] <- panels[[n]] + th_bottom

  # Equalize gtable column widths so left/right axis areas align perfectly
  grobs <- lapply(panels, ggplotGrob)
  max_w <- do.call(grid::unit.pmax, lapply(grobs, function(g) g$widths))
  for (i in seq_along(grobs)) grobs[[i]]$widths <- max_w
  gtable::gtable_matrix(
    "conditions",
    matrix(grobs, ncol = 1),
    widths  = grid::unit(1, "null"),
    heights = grid::unit(rep(1, length(grobs)), "null")
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
# 3. Cumulative precip YTD — all years spaghetti + quantile ribbons (ggplot2)
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
    mutate(plot_date = as.Date(paste0("2000-", doy), format = "%Y-%j"))

  hist_quants <- cumprec |>
    filter(cal_year < today_year) |>
    group_by(doy, plot_date) |>
    summarise(
      p05 = quantile(cum_precip, 0.05, na.rm = TRUE),
      p25 = quantile(cum_precip, 0.25, na.rm = TRUE),
      p50 = quantile(cum_precip, 0.50, na.rm = TRUE),
      p75 = quantile(cum_precip, 0.75, na.rm = TRUE),
      p95 = quantile(cum_precip, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  hist_lines <- cumprec |> filter(cal_year < today_year)
  cur_line   <- cumprec |> filter(cal_year == today_year)

  ggplot(hist_quants, aes(x = plot_date)) +
    geom_ribbon(aes(ymin = p05, ymax = p95), fill = "#cccccc", alpha = 0.30) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#999999", alpha = 0.40) +
    geom_line(aes(y = p50), color = "#666666", linewidth = 0.8) +
    geom_line(data = hist_lines,
              aes(y = cum_precip, group = cal_year),
              color = "#cccccc", linewidth = 0.3, alpha = 0.7) +
    geom_line(data = cur_line, aes(y = cum_precip),
              color = "#d44000", linewidth = 1.8) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(x = NULL, y = "Cumulative Precipitation (mm)",
         title = "Cumulative Precipitation \u2014 All Years",
         subtitle = glue("Bold = {today_year}. Bands = 25\u201375th & 5\u201395th percentile. Line = median.")) +
    theme_trailpulse() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
}

# ────────────────────────────────────────────────────────────────────────────
# 4. Soil wetness history — all years spaghetti + quantile ribbons (ggplot2)
# ────────────────────────────────────────────────────────────────────────────
plot_soil_moisture_history <- function(weather_df) {
  today_year <- year(Sys.Date())

  sm <- weather_df |>
    filter(!is.na(soil_moisture_0_1), source == "history" | date <= Sys.Date()) |>
    mutate(
      soil_wetness = 0.50 * soil_moisture_0_1 +
                     0.35 * coalesce(soil_moisture_1_3, soil_moisture_0_1) +
                     0.15 * coalesce(soil_moisture_3_9, soil_moisture_0_1),
      cal_year  = year(date),
      doy       = yday(date),
      plot_date = as.Date(paste0("2000-", doy), format = "%Y-%j")
    )

  hist_quants <- sm |>
    filter(cal_year < today_year) |>
    group_by(doy, plot_date) |>
    summarise(
      p05 = quantile(soil_wetness, 0.05, na.rm = TRUE),
      p25 = quantile(soil_wetness, 0.25, na.rm = TRUE),
      p50 = quantile(soil_wetness, 0.50, na.rm = TRUE),
      p75 = quantile(soil_wetness, 0.75, na.rm = TRUE),
      p95 = quantile(soil_wetness, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  hist_lines <- sm |> filter(cal_year < today_year)
  cur_line   <- sm |> filter(cal_year == today_year)

  ggplot(hist_quants, aes(x = plot_date)) +
    geom_ribbon(aes(ymin = p05, ymax = p95), fill = "#c8a47a", alpha = 0.25) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#9b6b3c", alpha = 0.35) +
    geom_line(aes(y = p50), color = "#6B3B18", linewidth = 0.8) +
    geom_line(data = hist_lines,
              aes(y = soil_wetness, group = cal_year),
              color = "#c8a47a", linewidth = 0.3, alpha = 0.6) +
    geom_line(data = cur_line, aes(y = soil_wetness),
              color = "#6B3B18", linewidth = 1.8) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = NULL, y = "Soil Moisture (m\u00b3/m\u00b3)",
         title = "Soil Moisture \u2014 Top 9 cm",
         subtitle = glue("Bold = {today_year}. Bands = 25\u201375th & 5\u201395th percentile. Line = median.")) +
    theme_trailpulse() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
}

# ────────────────────────────────────────────────────────────────────────────
# 5. Daily high temperature — all years spaghetti + quantile ribbons (ggplot2)
# ────────────────────────────────────────────────────────────────────────────
plot_temperature_history <- function(weather_df) {
  today_year <- year(Sys.Date())
  c2f <- function(c) c * 9/5 + 32

  tmp <- weather_df |>
    filter(!is.na(temp_max), source == "history" | date <= Sys.Date()) |>
    mutate(cal_year   = year(date),
           doy        = yday(date),
           plot_date  = as.Date(paste0("2000-", doy), format = "%Y-%j"),
           temp_max_f = c2f(temp_max))

  hist_quants <- tmp |>
    filter(cal_year < today_year) |>
    group_by(doy, plot_date) |>
    summarise(
      p05 = quantile(temp_max_f, 0.05, na.rm = TRUE),
      p25 = quantile(temp_max_f, 0.25, na.rm = TRUE),
      p50 = quantile(temp_max_f, 0.50, na.rm = TRUE),
      p75 = quantile(temp_max_f, 0.75, na.rm = TRUE),
      p95 = quantile(temp_max_f, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  hist_lines <- tmp |> filter(cal_year < today_year)
  cur_line   <- tmp |> filter(cal_year == today_year)

  ggplot(hist_quants, aes(x = plot_date)) +
    geom_ribbon(aes(ymin = p05, ymax = p95), fill = "#f4b97a", alpha = 0.25) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#e07030", alpha = 0.35) +
    geom_line(aes(y = p50), color = "#c04010", linewidth = 0.8) +
    geom_line(data = hist_lines,
              aes(y = temp_max_f, group = cal_year),
              color = "#f4b97a", linewidth = 0.3, alpha = 0.6) +
    geom_line(data = cur_line, aes(y = temp_max_f),
              color = "#c04010", linewidth = 1.8) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(x = NULL, y = "Daily High (\u00b0F)",
         title = "Daily High Temperature \u2014 All Years",
         subtitle = glue("Bold = {today_year}. Bands = 25\u201375th & 5\u201395th percentile. Line = median.")) +
    theme_trailpulse() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
}

# ────────────────────────────────────────────────────────────────────────────
# 6. Combined historical conditions — 4-panel patchwork
#    (temperature / cumulative precip / soil wetness / streamflow)
# ────────────────────────────────────────────────────────────────────────────
plot_historical_conditions <- function(weather_df, stream_df) {
  p_soil   <- plot_soil_moisture_history(weather_df)
  p_precip <- plot_cumulative_precip_ytd(weather_df)
  p_temp   <- plot_temperature_history(weather_df)
  p_stream <- plot_streamflow(stream_df)

  (p_soil / p_precip / p_temp / p_stream) +
    patchwork::plot_layout(heights = c(1, 1, 1, 1))
}

# ────────────────────────────────────────────────────────────────────────────
# 7. Streamflow — all water years spaghetti + quantile ribbons (ggplot2)
# ────────────────────────────────────────────────────────────────────────────
plot_streamflow <- function(stream_df) {
  today_cy <- year(Sys.Date())

  sf <- stream_df |>
    filter(!is.na(roll7_cfs)) |>
    mutate(cal_year  = year(date),
           doy       = yday(date),
           plot_date = as.Date(paste0("2000-", doy), format = "%Y-%j"))

  hist_quants <- sf |>
    filter(cal_year < today_cy) |>
    group_by(doy, plot_date) |>
    summarise(
      p05 = quantile(roll7_cfs, 0.05, na.rm = TRUE),
      p25 = quantile(roll7_cfs, 0.25, na.rm = TRUE),
      p50 = quantile(roll7_cfs, 0.50, na.rm = TRUE),
      p75 = quantile(roll7_cfs, 0.75, na.rm = TRUE),
      p95 = quantile(roll7_cfs, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  hist_lines <- sf |> filter(cal_year < today_cy)
  cur_line   <- sf |> filter(cal_year == today_cy)

  ggplot(hist_quants, aes(x = plot_date)) +
    geom_ribbon(aes(ymin = p05, ymax = p95), fill = "#90c4e4", alpha = 0.25) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#5b9fc7", alpha = 0.35) +
    geom_line(aes(y = p50), color = "#1565C0", linewidth = 0.8) +
    geom_line(data = hist_lines,
              aes(y = roll7_cfs, group = cal_year),
              color = "#90c4e4", linewidth = 0.3, alpha = 0.6) +
    geom_line(data = cur_line, aes(y = roll7_cfs),
              color = "#1565C0", linewidth = 1.8) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_log10(labels = scales::comma) +
    labs(x = NULL, y = "Discharge (cfs, log scale)",
         title = "Streamflow \u2014 All Calendar Years",
         subtitle = glue("Bold = {today_cy}. Bands = 25\u201375th & 5\u201395th percentile. Line = median.")) +
    theme_trailpulse() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
}

# ────────────────────────────────────────────────────────────────────────────
# 8. NDVI (HLS Landsat + Sentinel-2) — all years spaghetti + quantile ribbons
# ────────────────────────────────────────────────────────────────────────────
plot_ndvi_annual <- function(ndvi_df) {
  if (is.null(ndvi_df) || nrow(ndvi_df) == 0) return(invisible(NULL))

  today_year <- year(Sys.Date())

  nd <- ndvi_df |>
    mutate(
      cal_year  = year(date),
      doy       = yday(date),
      plot_date = as.Date(paste0("2000-", doy), format = "%Y-%j")
    )

  hist_quants <- nd |>
    filter(cal_year < today_year) |>
    group_by(doy, plot_date) |>
    summarise(
      p05 = quantile(ndvi, 0.05, na.rm = TRUE),
      p25 = quantile(ndvi, 0.25, na.rm = TRUE),
      p50 = quantile(ndvi, 0.50, na.rm = TRUE),
      p75 = quantile(ndvi, 0.75, na.rm = TRUE),
      p95 = quantile(ndvi, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  hist_lines <- nd |> filter(cal_year < today_year)
  cur_line   <- nd |> filter(cal_year == today_year)

  ggplot(hist_quants, aes(x = plot_date)) +
    geom_line(
      data  = hist_lines,
      aes(y = ndvi, group = interaction(cal_year, sensor)),
      color = "#a8d08d", linewidth = 0.3, alpha = 0.5
    ) +
    geom_ribbon(aes(ymin = p05, ymax = p95), fill = "#a8d08d", alpha = 0.35) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#5aaa5a", alpha = 0.45) +
    geom_line(aes(y = p50), color = "#2e7d32", linewidth = 0.8) +
    {if (nrow(cur_line) > 0)
        geom_line(data = cur_line, aes(y = ndvi),
                  color = "#2e7d32", linewidth = 1.8)} +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(limits = c(-0.1, 1.0)) +
    labs(
      x        = NULL,
      y        = "NDVI",
      title    = "NDVI (HLS Landsat + Sentinel-2) \u2014 All Years",
      subtitle = glue(
        "Bold = {today_year}. Bands = 25\u201375th & 5\u201395th percentile. Line = median."
      )
    ) +
    theme_trailpulse() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
}

# ────────────────────────────────────────────────────────────────────────────
# 6. Cumulative snowfall — all seasons spaghetti + quantile ribbons (ggplot2)
# ────────────────────────────────────────────────────────────────────────────
plot_cumulative_snowfall <- function(weather_df) {
  today_sy <- if (month(Sys.Date()) >= 11) year(Sys.Date()) else year(Sys.Date()) - 1L

  cum_sf <- compute_cumulative_snowfall(weather_df) |>
    mutate(plot_date = as.Date("2000-11-01") + doss - 1L)

  hist_quants <- cum_sf |>
    filter(snow_year < today_sy) |>
    group_by(doss, plot_date) |>
    summarise(
      p05 = quantile(cum_snowfall_cm, 0.05, na.rm = TRUE),
      p25 = quantile(cum_snowfall_cm, 0.25, na.rm = TRUE),
      p50 = quantile(cum_snowfall_cm, 0.50, na.rm = TRUE),
      p75 = quantile(cum_snowfall_cm, 0.75, na.rm = TRUE),
      p95 = quantile(cum_snowfall_cm, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  hist_lines <- cum_sf |> filter(snow_year < today_sy)
  cur_line   <- cum_sf |> filter(snow_year == today_sy)

  ggplot(hist_quants, aes(x = plot_date)) +
    geom_ribbon(aes(ymin = p05, ymax = p95), fill = "#b0d4f1", alpha = 0.25) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#64a8d4", alpha = 0.35) +
    geom_line(aes(y = p50), color = "#1e88e5", linewidth = 0.8) +
    geom_line(data = hist_lines,
              aes(y = cum_snowfall_cm, group = snow_year),
              color = "#b0d4f1", linewidth = 0.3, alpha = 0.7) +
    geom_line(data = cur_line, aes(y = cum_snowfall_cm),
              color = "#1e88e5", linewidth = 1.8) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(x = NULL, y = "Cumulative Snowfall (cm)",
         title = "Cumulative Snowfall \u2014 All Seasons",
         subtitle = glue("Bold = {today_sy}\u2013{today_sy + 1}. Bands = 25\u201375th & 5\u201395th percentile. Line = median.")) +
    theme_trailpulse() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
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
    filter(date >= Sys.Date()) |>
    arrange(date) |>
    slice_head(n = 7)

  if (nrow(forecast_rows) == 0) return(htmltools::HTML(""))

  day_divs <- purrr::pmap(forecast_rows, function(date, condition_score, condition_label, temp_max, ...) {
    face_svg <- make_mudface(condition_score, size = "70px", label = FALSE, temp_c = temp_max)
    htmltools::tags$div(
      class = "tp-forecast-day",
      htmltools::tags$span(class = "day-label", format(date, "%a")),
      face_svg,
      htmltools::tags$span(
        class = "mud-label",
        if (is.na(condition_label)) "?" else condition_label
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

  day_divs <- purrr::pmap(forecast_rows, function(date, snow_condition_score, snow_condition_label, ...) {
    face_svg <- make_snowface(snow_condition_score, size = "70px", label = FALSE)
    htmltools::tags$div(
      class = "tp-forecast-day",
      htmltools::tags$span(class = "day-label", format(date, "%a")),
      face_svg,
      htmltools::tags$span(
        class = "mud-label",
        if (is.na(snow_condition_label)) "?" else snow_condition_label
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

  cards <- purrr::pmap(day_rows, function(period_name, temp_f, temp_c, precip_pct,
                                          wind_speed, short_forecast, icon_url, ...) {
    htmltools::tags$div(
      class = "nws-forecast-card",
      htmltools::tags$div(class = "nws-period-name", period_name),
      if (!is.na(icon_url))
        htmltools::tags$img(src = icon_url, width = "48px", height = "48px",
                            alt = short_forecast,
                            onerror = "this.style.display='none'"),
      htmltools::tags$div(
        class = "nws-temp",
        glue("{temp_f}\u00b0F"), htmltools::tags$br(),
        glue("{round(temp_c)}\u00b0C")
      ),
      if (!is.na(precip_pct) && precip_pct > 0)
        htmltools::tags$div(class = "nws-precip", glue("\U0001f4a7 {round(precip_pct)}%")),
      htmltools::tags$div(class = "nws-wind", wind_speed),
      htmltools::tags$div(class = "nws-desc", short_forecast)
    )
  })

  do.call(
    htmltools::div,
    c(list(class = "tp-forecast-strip", style = "flex-wrap:wrap; gap:0.75rem"),
      Filter(Negate(is.null), cards))
  )
}
