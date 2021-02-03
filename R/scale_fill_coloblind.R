scale_fill_colorblind <- function(palette = "bright", alpha = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this functionality", call. = FALSE)
  }

  palette <- colorblind_palettes(palette, ...)

  ggplot2::scale_fill_manual(values = palette, alpha)
}
