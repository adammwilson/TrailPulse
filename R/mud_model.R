# R/mud_model.R
# Compute a 0-10 mud level from Open-Meteo soil moisture data.
# Works on both historical and forecast rows in the weather tibble.
#
# Depends on: setup.R, parks_config.R, condition.R

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
#   → "Frozen" flag when the 3-day rolling mean of shallow soil temp ≤ 0°C
#     (soil_temp_0cm represents 0-7cm for history, ~6cm for forecast).
#     Single cold air-temp spikes do NOT trigger frozen.

compute_mud_level <- function(
    weather_df,
    midpoint            = MUD_MIDPOINT,
    steepness           = MUD_STEEPNESS,
    precip_boost        = MUD_PRECIP_BOOST,
    soil_moisture_scale = MUD_SOIL_SCALE
) {
  weather_df |>
    mutate(
      # Weighted soil wetness (0 - ~0.6 scale); site scale factor applied before sigmoid
      soil_wetness = case_when(
        !is.na(soil_moisture_0_1) ~
          soil_moisture_scale * (
            0.50 * soil_moisture_0_1 +
            0.35 * coalesce(soil_moisture_1_3, soil_moisture_0_1) +
            0.15 * coalesce(soil_moisture_3_9, soil_moisture_0_1)
          ),
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

      # Frozen flag: 3-day rolling mean of shallow soil temp ≤ 0°C.
      # Falls back to single-day soil temp for the first rows where rolling
      # mean is NA (start of record / short time series).
      roll3_soil_temp = zoo::rollmean(soil_temp_0cm, k = 3L,
                                      fill = NA, align = "right"),
      frozen = coalesce(roll3_soil_temp <= 0,
                        !is.na(soil_temp_0cm) & soil_temp_0cm <= 0,
                        FALSE),

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
      ),

      # ── Unified condition columns (condition.R) ────────────────────────
      condition_score     = mud_to_condition_score(mud_score, frozen),
      condition_label     = score_to_label(condition_score),
      # Frozen ground is walkable (Fair) even though score is 5.0 < 6
      go_recommendation   = if_else(frozen,
                                    TRUE,
                                    go_recommendation(condition_score, "mud"))
    )
}

# ── Convenience: today's mud level ──────────────────────────────────────────
today_mud <- function(weather_mud_df) {
  weather_mud_df |>
    filter(date == Sys.Date()) |>
    slice(1)
}

# mud_oneliner() removed — use condition_oneliner(mode="mud", ...) from condition.R
