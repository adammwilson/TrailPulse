# R/fetch_nws.R
# Fetch 7-day NWS forecast text and precipitation probability
# from api.weather.gov (JSON API v2).
# Returns a tidy tibble: one row per forecast period.
#
# Depends on: setup.R, parks_config.R

fetch_nws_forecast <- function(
    lat = PARK_LAT,
    lon = PARK_LON
) {
  # ── Step 1: resolve grid point ───────────────────────────────────────────
  message("Fetching NWS grid point for (", lat, ", ", lon, ")...")

  points_resp <- request(
    glue("https://api.weather.gov/points/{lat},{lon}")
  ) |>
    req_headers(
      "User-Agent"   = "TrailPulse/1.0 (https://github.com/adammwilson/TrailPulse)",
      "Accept"       = "application/geo+json"
    ) |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(points_resp) != 200) {
    warning("NWS points API returned status ", resp_status(points_resp))
    return(.nws_empty())
  }

  props        <- resp_body_json(points_resp)$properties
  forecast_url <- props$forecast

  # ── Step 2: fetch forecast ───────────────────────────────────────────────
  message("Fetching NWS 7-day forecast from ", forecast_url)

  fc_resp <- request(forecast_url) |>
    req_headers(
      "User-Agent" = "TrailPulse/1.0 (https://github.com/adammwilson/TrailPulse)",
      "Accept"     = "application/geo+json"
    ) |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(fc_resp) != 200) {
    warning("NWS forecast API returned status ", resp_status(fc_resp))
    return(.nws_empty())
  }

  periods <- resp_body_json(fc_resp)$properties$periods

  tibble(
    period_name       = map_chr(periods, "name"),
    start_time        = map_chr(periods, "startTime") |> ymd_hms(quiet = TRUE),
    end_time          = map_chr(periods, "endTime")   |> ymd_hms(quiet = TRUE),
    is_daytime        = map_lgl(periods, "isDaytime"),
    temp_f            = map_int(periods, "temperature"),
    temp_c            = (temp_f - 32) * 5 / 9,
    precip_pct        = map(periods, "probabilityOfPrecipitation") |>
                          map_dbl(\(x) x$value %||% NA_real_),
    wind_speed        = map_chr(periods, "windSpeed"),
    short_forecast    = map_chr(periods, "shortForecast"),
    detailed_forecast = map_chr(periods, "detailedForecast"),
    icon_url          = map_chr(periods, "icon"),
    date              = as.Date(start_time)
  )
}

.nws_empty <- function() {
  tibble(
    period_name = character(), start_time = as.POSIXct(character()),
    end_time = as.POSIXct(character()), is_daytime = logical(),
    temp_f = integer(), temp_c = numeric(), precip_pct = numeric(),
    wind_speed = character(), short_forecast = character(),
    detailed_forecast = character(), icon_url = character(),
    date = as.Date(character())
  )
}
