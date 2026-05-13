# R/snowface.R
# Generate an inline SVG nordic skier whose expression gets happier and
# snowier as snow depth increases.
# 
# States (by snow depth, cm):
#   0       Bare / Bored      → sad skier, bare headband, dry poles, no snow
#   1-5     Dusting           → neutral, light dusting on shoulders
#   6-15    Skiable           → smile, snow on headband + shoulders, poles in snow
#   16-30   Good              → big grin, heavy snow, powder spray off poles
#   31+     Powder Day        → ecstatic, jumping, deep snow, huge grin
#
# The skier is a simple front-facing figure: round head with a headband,
# body, poles on either side. Snow accumulates progressively.
#
# Depends on: htmltools, glue

make_snowface <- function(snow_depth_cm, size = "160px", label = TRUE) {

  depth <- coalesce(as.numeric(snow_depth_cm), 0)

  state <- dplyr::case_when(
    depth <= 0   ~ 0L,
    depth < 5    ~ 1L,
    depth < 15   ~ 2L,
    depth < 30   ~ 3L,
    TRUE         ~ 4L
  )

  # ── Base figure ──────────────────────────────────────────────────────────
  # Coordinate system: 0-160 × 0-160
  # Head centered at (80, 62), body below

  sky_color   <- c("#e0eaf5", "#d0dfee", "#b8d0ea", "#9dbfe4", "#7aaad8")[state + 1]
  snow_ground <- c("none",    "#e8f4fb", "#dbeefa", "#c5e4f7", "#b0d8f5")[state + 1]

  base <- glue('
    <!-- Sky background -->
    <rect width="160" height="160" fill="{sky_color}"/>
    <!-- Ground snow -->
    <ellipse cx="80" cy="148" rx="70" ry="14" fill="{snow_ground}" opacity="0.9"/>
  ')

  # ── Poles ─────────────────────────────────────────────────────────────────
  # Poles angle outward from hands; hands at ~(42, 108) and (118, 108)
  # State 4: poles are raised in triumph

  poles <- switch(as.character(state),
    "0" = '<!-- Poles resting, no snow -->
           <line x1="32" y1="100" x2="25" y2="148" stroke="#888" stroke-width="3" stroke-linecap="round"/>
           <line x1="128" y1="100" x2="135" y2="148" stroke="#888" stroke-width="3" stroke-linecap="round"/>
           <!-- Baskets -->
           <circle cx="25" cy="148" r="4" fill="#888" opacity="0.7"/>
           <circle cx="135" cy="148" r="4" fill="#888" opacity="0.7"/>',

    "1" = '<!-- Poles in light snow -->
           <line x1="32" y1="100" x2="24" y2="148" stroke="#888" stroke-width="3" stroke-linecap="round"/>
           <line x1="128" y1="100" x2="136" y2="148" stroke="#888" stroke-width="3" stroke-linecap="round"/>
           <circle cx="24" cy="145" r="4" fill="#888" opacity="0.7"/>
           <circle cx="136" cy="145" r="4" fill="#888" opacity="0.7"/>
           <!-- Dusting on pole tips -->
           <circle cx="24" cy="147" r="3" fill="white" opacity="0.7"/>
           <circle cx="136" cy="147" r="3" fill="white" opacity="0.7"/>',

    "2" = '<!-- Poles planted in snow, forward lean -->
           <line x1="35" y1="98" x2="22" y2="145" stroke="#777" stroke-width="3.5" stroke-linecap="round"/>
           <line x1="125" y1="98" x2="138" y2="145" stroke="#777" stroke-width="3.5" stroke-linecap="round"/>
           <circle cx="22" cy="143" r="5" fill="#777" opacity="0.8"/>
           <circle cx="138" cy="143" r="5" fill="#777" opacity="0.8"/>
           <!-- Snow on baskets -->
           <ellipse cx="22" cy="145" rx="6" ry="3" fill="white" opacity="0.8"/>
           <ellipse cx="138" cy="145" rx="6" ry="3" fill="white" opacity="0.8"/>',

    "3" = '<!-- Poles dynamic push, heavy snow -->
           <line x1="38" y1="96" x2="18" y2="140" stroke="#666" stroke-width="4" stroke-linecap="round"/>
           <line x1="122" y1="96" x2="142" y2="140" stroke="#666" stroke-width="4" stroke-linecap="round"/>
           <!-- Powder spray off baskets -->
           <ellipse cx="18" cy="138" rx="9" ry="4" fill="white" opacity="0.85" transform="rotate(-20,18,138)"/>
           <ellipse cx="142" cy="138" rx="9" ry="4" fill="white" opacity="0.85" transform="rotate(20,142,138)"/>',

    # State 4 – poles raised triumphantly
    '<!-- Poles raised in triumph -->
     <line x1="42" y1="100" x2="22" y2="55" stroke="#555" stroke-width="4" stroke-linecap="round"/>
     <line x1="118" y1="100" x2="138" y2="55" stroke="#555" stroke-width="4" stroke-linecap="round"/>
     <!-- Powder spray at tips -->
     <ellipse cx="22" cy="53" rx="10" ry="5" fill="white" opacity="0.9" transform="rotate(-30,22,53)"/>
     <ellipse cx="138" cy="53" rx="10" ry="5" fill="white" opacity="0.9" transform="rotate(30,138,53)"/>'
  )

  # ── Body ─────────────────────────────────────────────────────────────────
  jacket_color <- c("#5a7a9a", "#4a6e8c", "#3d6282", "#2f5372", "#1f3f5a")[state + 1]

  body <- glue('
    <!-- Body / jacket -->
    <ellipse cx="80" cy="118" rx="30" ry="26" fill="{jacket_color}"/>
    <!-- Arms -->
    <ellipse cx="42" cy="105" rx="14" ry="7" fill="{jacket_color}" transform="rotate(-20,42,105)"/>
    <ellipse cx="118" cy="105" rx="14" ry="7" fill="{jacket_color}" transform="rotate(20,118,105)"/>
    <!-- Hands -->
    <circle cx="32" cy="100" r="7" fill="#FDDBB4"/>
    <circle cx="128" cy="100" r="7" fill="#FDDBB4"/>
    <!-- Ski pants -->
    <rect x="62" y="136" width="16" height="12" rx="3" fill="#2a3f5a"/>
    <rect x="82" y="136" width="16" height="12" rx="3" fill="#2a3f5a"/>
    <!-- Skis -->
    <ellipse cx="70" cy="150" rx="22" ry="4" fill="#8B6914" transform="rotate(-5,70,150)"/>
    <ellipse cx="90" cy="150" rx="22" ry="4" fill="#8B6914" transform="rotate(5,90,150)"/>
  ')

  # ── Head ─────────────────────────────────────────────────────────────────
  head <- '
    <!-- Head -->
    <circle cx="80" cy="60" r="30" fill="#FDDBB4" stroke="#C8975A" stroke-width="2"/>
    <!-- Hair (simple fringe) -->
    <path d="M52,50 Q57,35 68,38 Q75,32 80,35 Q85,32 92,38 Q103,35 108,50"
          fill="#5a3a1a" stroke="none"/>
  '

  # ── Headband ─────────────────────────────────────────────────────────────
  headband_colors <- c("#cc3333", "#cc3333", "#cc3333", "#cc3333", "#cc3333")
  hb_color <- headband_colors[state + 1]

  snow_on_headband <- switch(as.character(state),
    "0" = "",
    "1" = '<ellipse cx="80" cy="38" rx="12" ry="3" fill="white" opacity="0.7"/>',
    "2" = '<ellipse cx="80" cy="37" rx="18" ry="4" fill="white" opacity="0.8"/>',
    "3" = '<ellipse cx="80" cy="36" rx="24" ry="5" fill="white" opacity="0.85"/>
           <circle cx="60" cy="38" r="4" fill="white" opacity="0.7"/>
           <circle cx="100" cy="38" r="4" fill="white" opacity="0.7"/>',
    '<ellipse cx="80" cy="35" rx="28" ry="6" fill="white" opacity="0.9"/>
     <circle cx="55" cy="38" r="5" fill="white" opacity="0.75"/>
     <circle cx="105" cy="38" r="5" fill="white" opacity="0.75"/>
     <!-- Snow piling on top of head -->
     <ellipse cx="80" cy="30" rx="20" ry="7" fill="white" opacity="0.85"/>'
  )

  headband <- glue('
    <!-- Headband -->
    <path d="M50,46 Q80,35 110,46 Q110,52 80,41 Q50,52 50,46"
          fill="{hb_color}" opacity="0.9"/>
    {snow_on_headband}
  ')

  # ── Eyes ─────────────────────────────────────────────────────────────────
  eyes <- switch(state + 1L,
    # 0 – sad, drooping
    '<!-- Sad eyes -->
     <circle cx="67" cy="62" r="7" fill="white"/>
     <circle cx="93" cy="62" r="7" fill="white"/>
     <circle cx="67" cy="64" r="4" fill="#333"/>
     <circle cx="93" cy="64" r="4" fill="#333"/>
     <circle cx="68" cy="63" r="1.5" fill="white"/>
     <circle cx="94" cy="63" r="1.5" fill="white"/>
     <!-- Drooping inner brows -->
     <line x1="60" y1="53" x2="74" y2="57" stroke="#5a3a1a" stroke-width="2.5" stroke-linecap="round"/>
     <line x1="86" y1="57" x2="100" y2="53" stroke="#5a3a1a" stroke-width="2.5" stroke-linecap="round"/>',

    # 1 – neutral
    '<!-- Neutral eyes -->
     <circle cx="67" cy="62" r="7" fill="white"/>
     <circle cx="93" cy="62" r="7" fill="white"/>
     <circle cx="67" cy="62" r="4" fill="#333"/>
     <circle cx="93" cy="62" r="4" fill="#333"/>
     <circle cx="68" cy="61" r="1.5" fill="white"/>
     <circle cx="94" cy="61" r="1.5" fill="white"/>',

    # 2 – smile, bright eyes
    '<!-- Happy eyes -->
     <circle cx="67" cy="61" r="7" fill="white"/>
     <circle cx="93" cy="61" r="7" fill="white"/>
     <circle cx="67" cy="61" r="4" fill="#333"/>
     <circle cx="93" cy="61" r="4" fill="#333"/>
     <circle cx="68.5" cy="59.5" r="1.5" fill="white"/>
     <circle cx="94.5" cy="59.5" r="1.5" fill="white"/>',

    # 3 – big grin, wide eyes
    '<!-- Wide excited eyes -->
     <circle cx="67" cy="60" r="8" fill="white"/>
     <circle cx="93" cy="60" r="8" fill="white"/>
     <circle cx="67" cy="60" r="5" fill="#333"/>
     <circle cx="93" cy="60" r="5" fill="#333"/>
     <circle cx="69" cy="58" r="2" fill="white"/>
     <circle cx="95" cy="58" r="2" fill="white"/>',

    # 4 – ecstatic, crinkled happy eyes
    '<!-- Crinkled ecstatic eyes -->
     <path d="M59,60 Q67,53 75,60" fill="none" stroke="#333" stroke-width="3" stroke-linecap="round"/>
     <path d="M85,60 Q93,53 101,60" fill="none" stroke="#333" stroke-width="3" stroke-linecap="round"/>
     <!-- Happy cheeks -->
     <circle cx="57" cy="67" r="8" fill="#FFB6A3" opacity="0.5"/>
     <circle cx="103" cy="67" r="8" fill="#FFB6A3" opacity="0.5"/>'
  )

  # ── Mouths ───────────────────────────────────────────────────────────────
  mouths <- switch(state + 1L,
    # 0 – drooping frown
    '<path d="M65,78 Q80,73 95,78" fill="none" stroke="#333" stroke-width="3" stroke-linecap="round"/>',

    # 1 – flat
    '<path d="M65,76 Q80,79 95,76" fill="none" stroke="#333" stroke-width="2.5" stroke-linecap="round"/>',

    # 2 – smile
    '<path d="M64,74 Q80,85 96,74" fill="none" stroke="#333" stroke-width="3" stroke-linecap="round"/>',

    # 3 – big smile with teeth
    '<path d="M62,73 Q80,88 98,73" fill="none" stroke="#333" stroke-width="3.5" stroke-linecap="round"/>
     <path d="M67,75 Q80,86 93,75" fill="white" stroke="none"/>',

    # 4 – huge open grin
    '<path d="M60,72 Q80,92 100,72" fill="none" stroke="#333" stroke-width="4" stroke-linecap="round"/>
     <ellipse cx="80" cy="79" rx="15" ry="8" fill="white" opacity="0.9"/>
     <!-- Tongue -->
     <ellipse cx="80" cy="84" rx="8" ry="5" fill="#FFB6A3"/>'
  )

  # ── Snow on shoulders ────────────────────────────────────────────────────
  shoulder_snow <- switch(as.character(state),
    "0" = "",
    "1" = '<ellipse cx="50" cy="100" rx="10" ry="4" fill="white" opacity="0.7"/>
           <ellipse cx="110" cy="100" rx="10" ry="4" fill="white" opacity="0.7"/>',
    "2" = '<ellipse cx="48" cy="98" rx="14" ry="5" fill="white" opacity="0.8"/>
           <ellipse cx="112" cy="98" rx="14" ry="5" fill="white" opacity="0.8"/>',
    "3" = '<ellipse cx="46" cy="97" rx="18" ry="7" fill="white" opacity="0.85"/>
           <ellipse cx="114" cy="97" rx="18" ry="7" fill="white" opacity="0.85"/>
           <!-- Powder cloud -->
           <ellipse cx="30" cy="92" rx="12" ry="6" fill="white" opacity="0.6"/>
           <ellipse cx="130" cy="92" rx="12" ry="6" fill="white" opacity="0.6"/>',
    '<ellipse cx="44" cy="96" rx="22" ry="9" fill="white" opacity="0.9"/>
     <ellipse cx="116" cy="96" rx="22" ry="9" fill="white" opacity="0.9"/>
     <!-- Deep powder around legs -->
     <ellipse cx="70" cy="145" rx="18" ry="8" fill="white" opacity="0.75"/>
     <ellipse cx="90" cy="145" rx="18" ry="8" fill="white" opacity="0.75"/>
     <!-- Powder spray -->
     <ellipse cx="20" cy="88" rx="15" ry="7" fill="white" opacity="0.65" transform="rotate(-15,20,88)"/>
     <ellipse cx="140" cy="88" rx="15" ry="7" fill="white" opacity="0.65" transform="rotate(15,140,88)"/>'
  )

  # ── Assemble SVG ─────────────────────────────────────────────────────────
  level_name <- snow_level_name(depth)

  svg_content <- paste0(base, poles, body, shoulder_snow, head, headband, eyes, mouths)

  svg <- glue(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 160 160"',
    ' width="{size}" height="{size}" role="img"',
    ' aria-label="Snow condition: {level_name}">',
    '{svg_content}',
    '</svg>'
  )

  if (label) {
    css_idx <- as.character(state)
    lbl <- glue(
      '<div style="text-align:center;margin-top:0.25rem;">',
      '<span class="tp-level-badge snow-{css_idx}">{level_name}</span>',
      '</div>'
    )
    svg <- paste0(svg, lbl)
  }

  htmltools::HTML(svg)
}
