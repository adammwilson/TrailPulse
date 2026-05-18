# R/snowface.R
# Returns an htmltools <img> tag for the appropriate snowface PNG icon.
# Icons live in img/snowface/snowface01.png – snowface04.png.
# Icon selection is based on the unified condition_score [1–10]:
#   ≤4 (Avoid/Poor) → icon 4 · ≤6 (Fair) → icon 3 · ≤8 (Good) → icon 2 · >8 (Great) → icon 1
#
# Depends on: htmltools, condition.R

make_snowface <- function(condition_score, size = "160px", label = TRUE) {
  cs    <- coalesce(as.numeric(condition_score), 1.0)
  n     <- if      (cs > 8) 1L
            else if (cs > 6) 2L
            else if (cs > 4) 3L
            else             4L

  src <- sprintf("../../img/snowface/snowface%02d.png", n)
  img <- htmltools::tags$img(
    src   = src,
    alt   = c("Great", "Good", "Fair", "Poor/Avoid")[n],
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

