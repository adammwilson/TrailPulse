# R/mudface.R
# Returns an htmltools <img> tag for the appropriate mudface PNG icon.
# Icons live in img/mudface/mudface01.png – mudface04.png.
# 4-level ranking: Great (<3) · Good (<5) · Poor (<7.5) · Awful (≥7.5)
#
# Depends on: htmltools

make_mudface <- function(mud_level, size = "160px", label = TRUE, temp_c = NULL) {
  ml <- if (is.null(mud_level) || is.na(mud_level)) 5 else mud_level
  n  <- if      (ml < 3)   1L
        else if (ml < 5)   2L
        else if (ml < 7.5) 3L
        else               4L

  src <- sprintf("../../img/mudface/mudface%02d.png", n)
  img <- htmltools::tags$img(
    src   = src,
    alt   = c("Great", "Good", "Poor", "Awful")[n],
    style = sprintf("width:%s;height:%s;object-fit:contain;", size, size)
  )

  if (label) {
    lbl <- htmltools::tags$div(
      style = "text-align:center;margin-top:0.25rem;",
      htmltools::tags$span(class = "tp-level-badge", c("Great", "Good", "Poor", "Awful")[n])
    )
    htmltools::tagList(img, lbl)
  } else {
    img
  }
}

