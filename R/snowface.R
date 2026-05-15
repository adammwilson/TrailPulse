# R/snowface.R
# Returns an htmltools <img> tag for the appropriate snowface PNG icon.
# Icons live in img/snowface/snowface01.png – snowface04.png.
# 4-level ranking: Great (≥25cm) · Good (≥10cm) · Poor (≥5cm) · Awful (<5cm)
#
# Depends on: htmltools

make_snowface <- function(snow_depth_cm, size = "160px", label = TRUE) {
  depth <- coalesce(as.numeric(snow_depth_cm), 0)
  n     <- if      (depth >= 25) 1L
            else if (depth >= 10) 2L
            else if (depth >=  5) 3L
            else                  4L

  src <- sprintf("../../img/snowface/snowface%02d.png", n)
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

