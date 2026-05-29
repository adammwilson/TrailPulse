# R/mudface.R
# Returns an htmltools <img> tag for the appropriate mudface PNG icon.
# Icons live in img/mudface/mudface01.png – mudface04.png.
# Icon selection is based on the unified condition_score [1–10]:
#   <2 (Avoid) → icon 4 · <6 (Poor) → icon 3 · <8 (Good) → icon 2 · ≥8 (Great) → icon 1
# Breakpoints match CONDITION_BREAKS in condition.R (2, 6, 8).
#
# Depends on: htmltools, condition.R

make_mudface <- function(condition_score, size = "160px", label = TRUE, temp_c = NULL) {
  cs <- if (is.null(condition_score) || is.na(condition_score)) 5.0
        else as.numeric(condition_score)
  n  <- if      (cs >= 8) 1L
        else if (cs >= 6) 2L
        else if (cs >= 2) 3L
        else              4L

  src <- sprintf("../../img/mudface/mudface%02d.png", n)
  img <- htmltools::tags$img(
    src   = src,
    alt   = c("Great", "Good", "Poor", "Poor/Avoid")[n],
    style = sprintf("width:%s;height:%s;object-fit:contain;", size, size)
  )

  if (label) {
    lbl_text <- score_to_label(cs)
    lbl <- htmltools::tags$div(
      style = "text-align:center;margin-top:0.25rem;",
      htmltools::tags$span(class = "tp-level-badge", lbl_text)
    )
    htmltools::tagList(img, lbl)
  } else {
    img
  }
}

