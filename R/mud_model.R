# R/mud_model.R
# Compute a 0-10 mud level from Open-Meteo soil moisture data.
# Works on both historical and forecast rows in the weather tibble.
#
# Depends on: setup.R, parks_config.R

# ── Core model ──────────────────────────────────────────────────────────────
#
# Soil wetness: weighted mean of top three soil layers (0-9 cm)
#   soil_wetness = 0.50 * sm_0_1 + 0.35 * sm_1_3 + 0.15 * sm_3_9
#   (values are volumetric water content, roughly 0-0.6 m³/m³)
#
# Mud score (0-10):  logistic sigmoid scaled to 0-10
#   mud_raw = 10 / (1 + exp(-steepness * (soil_wetness - midpoint)))
#
# Modifiers:
#   +precip_boost if precip > 5 mm in last 24h (surface saturation)
#   → "Frozen" flag if temp_max < 0 °C (frozen surface overrides mud)

compute_mud_level <- function(
    weather_df,
    midpoint     = MUD_MIDPOINT,
    steepness    = MUD_STEEPNESS,
    precip_boost = MUD_PRECIP_BOOST
) {
  weather_df |>
    mutate(
      # Weighted soil wetness (0 - ~0.6 scale)
      soil_wetness = case_when(
        !is.na(soil_moisture_0_1) ~
          0.50 * soil_moisture_0_1 +
          0.35 * coalesce(soil_moisture_1_3, soil_moisture_0_1) +
          0.15 * coalesce(soil_moisture_3_9, soil_moisture_0_1),
        TRUE ~ NA_real_
      ),

      # Logistic sigmoid → 0-10
      mud_raw = 10 / (1 + exp(-steepness * (soil_wetness - midpoint))),

      # Precipitation surface boost
      mud_score = case_when(
        is.na(mud_raw) ~ NA_real_,
        !is.na(precip) & precip > 5 ~ pmin(mud_raw + precip_boost, 10),
        TRUE ~ mud_raw
      ),

      # Frozen surface flag: cold enough that surface is frozen
      frozen = !is.na(temp_max) & temp_max < 0,

      # Final mud level: NA if frozen (show "Frozen" state instead)
      mud_level = if_else(frozen, NA_real_, mud_score),

      # Named level
      mud_level_name = case_when(
        frozen                  ~ "Frozen",
        is.na(mud_level)        ~ NA_character_,
        mud_level < 1           ~ "Bone Dry",
        mud_level < 2.5         ~ "Dusty",
        mud_level < 4           ~ "Firm",
        mud_level < 5.5         ~ "Tacky",
        mud_level < 6.75        ~ "Soft",
        mud_level < 7.75        ~ "Muddy",
        mud_level < 9           ~ "Very Muddy",
        TRUE                    ~ "Impassable"
      ),

      # CSS class for coloring (0-7 index)
      mud_css_class = case_when(
        frozen | is.na(mud_level) ~ "mud-frozen",
        mud_level < 1    ~ "mud-0",
        mud_level < 2.5  ~ "mud-1",
        mud_level < 4    ~ "mud-2",
        mud_level < 5.5  ~ "mud-3",
        mud_level < 6.75 ~ "mud-4",
        mud_level < 7.75 ~ "mud-5",
        mud_level < 9    ~ "mud-6",
        TRUE             ~ "mud-7"
      )
    )
}

# ── Convenience: today's mud level ──────────────────────────────────────────
today_mud <- function(weather_mud_df) {
  weather_mud_df |>
    filter(date == Sys.Date()) |>
    slice(1)
}

# ── One-liner summary text ───────────────────────────────────────────────────
mud_oneliner <- function(row, activities = c("hiking", "mountain_biking")) {
  lvl  <- row$mud_level_name
  if (is.na(lvl)) return("Condition data unavailable.")

  base <- switch(lvl,
    "Frozen"     = "Surface is frozen \u2014 watch for ice on shaded sections.",
    "Bone Dry"   = "Trails are bone dry. Perfect conditions.",
    "Dusty"      = "Trails are dusty but firm \u2014 great day to ride or hike.",
    "Firm"       = "Trails are firm with good traction \u2014 ideal conditions.",
    "Tacky"      = "Trails are tacky \u2014 expect excellent grip, light mud on low sections.",
    "Soft"       = "Trails are softening \u2014 low areas may have mud.",
    "Muddy"      = "Trails are muddy \u2014 consider staying on hard-packed surfaces.",
    "Very Muddy" = "Trails are very muddy \u2014 bikes will cause damage. Hike if you must.",
    "Impassable" = "Trails are impassable \u2014 please stay home to protect trail surfaces.",
    "No data."
  )

  if ("mountain_biking" %in% activities && lvl %in% c("Muddy", "Very Muddy", "Impassable")) {
    base <- paste(base, "\U1F6B5 Mountain biking is strongly discouraged.")
  }

  base
}
