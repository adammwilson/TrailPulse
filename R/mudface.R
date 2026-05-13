# R/mudface.R
# Generate an inline SVG mudface whose expression gets sadder and muddier
# as the mud level increases from 0-10.
#
# States (by mud_level):
#   0-2   Clean / Bone Dry  → big smile, clean face
#   2-4   Light Mud         → neutral smile, light mud dots
#   4-6   Muddy             → mild frown, scattered patches
#   6-8   Very Muddy        → deep frown, heavy coverage
#   8-10  Impassable        → crying, eyes shut, face fully covered
#
# Depends on: htmltools

make_mudface <- function(mud_level, size = "160px", label = TRUE) {

  # Map numeric score to state 0-4
  state <- if (is.na(mud_level)) {
    -1L  # frozen / no data
  } else {
    as.integer(min(floor(mud_level / 2), 4L))
  }

  # ── Shared face geometry ─────────────────────────────────────────────────
  # Face circle, eyes, nose — same for all states; mouth and mud change.

  face_base <- '
    <!-- Face circle -->
    <circle cx="80" cy="80" r="70" fill="#FDDBB4" stroke="#C8975A" stroke-width="3"/>
    <!-- Eyes (left, right) - base whites -->
    <circle cx="55" cy="68" r="12" fill="white"/>
    <circle cx="105" cy="68" r="12" fill="white"/>
    <!-- Nose -->
    <ellipse cx="80" cy="85" rx="5" ry="7" fill="#C8975A" opacity="0.5"/>'

  # ── State-specific eyes ──────────────────────────────────────────────────
  eyes <- list(
    # State 0 – happy, open eyes
    '<!-- Pupils happy -->
     <circle cx="55" cy="68" r="6" fill="#333"/>
     <circle cx="105" cy="68" r="6" fill="#333"/>
     <circle cx="57" cy="66" r="2" fill="white"/>
     <circle cx="107" cy="66" r="2" fill="white"/>',

    # State 1 – neutral
    '<!-- Pupils neutral -->
     <circle cx="55" cy="68" r="6" fill="#333"/>
     <circle cx="105" cy="68" r="6" fill="#333"/>
     <circle cx="57" cy="66" r="2" fill="white"/>
     <circle cx="107" cy="66" r="2" fill="white"/>',

    # State 2 – mild frown
    '<!-- Pupils mild frown -->
     <circle cx="55" cy="70" r="6" fill="#333"/>
     <circle cx="105" cy="70" r="6" fill="#333"/>
     <!-- Furrowed brow left -->
     <line x1="44" y1="55" x2="66" y2="59" stroke="#8B5E3C" stroke-width="3" stroke-linecap="round"/>
     <!-- Furrowed brow right -->
     <line x1="94" y1="59" x2="116" y2="55" stroke="#8B5E3C" stroke-width="3" stroke-linecap="round"/>',

    # State 3 – deep frown
    '<!-- Pupils worried -->
     <circle cx="55" cy="72" r="6" fill="#333"/>
     <circle cx="105" cy="72" r="6" fill="#333"/>
     <!-- Heavy brow left -->
     <line x1="42" y1="53" x2="67" y2="60" stroke="#5A3A1A" stroke-width="4" stroke-linecap="round"/>
     <!-- Heavy brow right -->
     <line x1="93" y1="60" x2="118" y2="53" stroke="#5A3A1A" stroke-width="4" stroke-linecap="round"/>',

    # State 4 – crying, eyes closed
    '<!-- Eyes closed / crying -->
     <!-- Closed eye lines -->
     <path d="M44,68 Q55,75 66,68" fill="none" stroke="#333" stroke-width="3" stroke-linecap="round"/>
     <path d="M94,68 Q105,75 116,68" fill="none" stroke="#333" stroke-width="3" stroke-linecap="round"/>
     <!-- Tear drops -->
     <ellipse cx="50" cy="80" rx="3" ry="5" fill="#7EB8D4" opacity="0.9"/>
     <ellipse cx="110" cy="80" rx="3" ry="5" fill="#7EB8D4" opacity="0.9"/>
     <!-- Very heavy brow -->
     <line x1="42" y1="52" x2="67" y2="61" stroke="#3A2010" stroke-width="5" stroke-linecap="round"/>
     <line x1="93" y1="61" x2="118" y2="52" stroke="#3A2010" stroke-width="5" stroke-linecap="round"/>'
  )

  # ── State-specific mouths ────────────────────────────────────────────────
  mouths <- list(
    # State 0 – big smile
    '<path d="M52,100 Q80,125 108,100" fill="none" stroke="#333" stroke-width="4" stroke-linecap="round"/>
     <!-- Teeth -->
     <path d="M58,103 Q80,118 102,103" fill="white" stroke="none"/>',

    # State 1 – slight smile
    '<path d="M58,103 Q80,115 102,103" fill="none" stroke="#333" stroke-width="3.5" stroke-linecap="round"/>',

    # State 2 – flat/slight frown
    '<path d="M58,107 Q80,100 102,107" fill="none" stroke="#333" stroke-width="3.5" stroke-linecap="round"/>',

    # State 3 – clear frown
    '<path d="M54,112 Q80,98 106,112" fill="none" stroke="#333" stroke-width="4" stroke-linecap="round"/>',

    # State 4 – deep frown / open crying mouth
    '<path d="M52,115 Q80,97 108,115" fill="none" stroke="#333" stroke-width="4.5" stroke-linecap="round"/>
     <!-- Open mouth oval -->
     <ellipse cx="80" cy="113" rx="16" ry="9" fill="#9B3A3A" opacity="0.7"/>'
  )

  # ── Mud splatter paths (cumulative by state) ──────────────────────────────
  mud_color <- "#6B3B18"
  mud_splats <- list(
    # State 0 – none
    "",

    # State 1 – light dots
    gsub("%s", mud_color, '<!-- Light mud dots -->
     <circle cx="65" cy="90" r="4" fill="%s" opacity="0.6"/>
     <circle cx="96" cy="87" r="3" fill="%s" opacity="0.55"/>
     <circle cx="75" cy="55" r="2.5" fill="%s" opacity="0.5"/>
     <circle cx="100" cy="100" r="3.5" fill="%s" opacity="0.5"/>'),
    # fixed: gsub replaces all %s with mud_color

    # State 2 – scattered patches
    gsub("%s", mud_color, '<!-- Scattered mud patches -->
     <ellipse cx="60" cy="88" rx="7" ry="5" fill="%s" opacity="0.65"/>
     <ellipse cx="100" cy="85" rx="6" ry="4" fill="%s" opacity="0.6"/>
     <circle cx="75" cy="52" r="5" fill="%s" opacity="0.55"/>
     <ellipse cx="45" cy="75" rx="5" ry="4" fill="%s" opacity="0.5"/>
     <ellipse cx="110" cy="100" rx="6" ry="4" fill="%s" opacity="0.55"/>
     <circle cx="80" cy="42" r="4" fill="%s" opacity="0.45"/>'),
    # fixed: gsub replaces all %s with mud_color

    # State 3 – heavy coverage
    gsub("%s", mud_color, '<!-- Heavy mud coverage -->
     <ellipse cx="55" cy="85" rx="12" ry="9" fill="%s" opacity="0.75"/>
     <ellipse cx="105" cy="82" rx="11" ry="8" fill="%s" opacity="0.7"/>
     <ellipse cx="80" cy="48" rx="9" ry="6" fill="%s" opacity="0.65"/>
     <ellipse cx="42" cy="68" rx="8" ry="6" fill="%s" opacity="0.6"/>
     <ellipse cx="118" cy="95" rx="9" ry="6" fill="%s" opacity="0.65"/>
     <ellipse cx="70" cy="102" rx="8" ry="5" fill="%s" opacity="0.6"/>
     <ellipse cx="92" cy="105" rx="7" ry="5" fill="%s" opacity="0.55"/>'),
    # fixed: gsub replaces all %s with mud_color

    # State 4 – fully covered (large blobs, extra layer)
    gsub("%s", mud_color, '<!-- Full mud coverage -->
     <ellipse cx="80" cy="80" rx="65" ry="55" fill="%s" opacity="0.3"/>
     <ellipse cx="50" cy="75" rx="18" ry="14" fill="%s" opacity="0.7"/>
     <ellipse cx="112" cy="70" rx="16" ry="12" fill="%s" opacity="0.7"/>
     <ellipse cx="80" cy="45" rx="14" ry="9" fill="%s" opacity="0.7"/>
     <ellipse cx="38" cy="90" rx="12" ry="9" fill="%s" opacity="0.65"/>
     <ellipse cx="122" cy="100" rx="11" ry="8" fill="%s" opacity="0.65"/>
     <ellipse cx="65" cy="110" rx="10" ry="7" fill="%s" opacity="0.65"/>
     <ellipse cx="95" cy="112" rx="10" ry="7" fill="%s" opacity="0.6"/>') # state 4
  )

  # ── Frozen state ─────────────────────────────────────────────────────────
  if (state == -1L) {
    svg_content <- paste0(
      face_base,
      eyes[[2]],  # neutral eyes
      mouths[[2]], # flat mouth
      # Ice crystals
      '<text x="55" y="85" font-size="20" opacity="0.7">❄</text>',
      '<text x="85" y="58" font-size="14" opacity="0.6">❄</text>',
      '<text x="98" y="100" font-size="16" opacity="0.65">❄</text>'
    )
    face_label <- "Frozen"
  } else {
    idx <- state + 1L
    svg_content <- paste0(face_base, mud_splats[[idx]], eyes[[idx]], mouths[[idx]])
    face_label <- switch(as.character(state),
      "0" = "Bone Dry",
      "1" = "Light Mud",
      "2" = "Muddy",
      "3" = "Very Muddy",
      "4" = "Impassable"
    )
  }

  svg <- glue(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 160 160"',
    ' width="{size}" height="{size}" role="img"',
    ' aria-label="Mud condition: {face_label}">',
    '{svg_content}',
    '</svg>'
  )

  if (label) {
    css_idx <- if (is.na(mud_level)) "frozen" else as.character(min(floor(mud_level / 10 * 7), 7))
    lbl <- glue(
      '<div style="text-align:center;margin-top:0.25rem;">',
      '<span class="tp-level-badge mud-{css_idx}">{face_label}</span>',
      '</div>'
    )
    svg <- paste0(svg, lbl)
  }

  htmltools::HTML(svg)
}
