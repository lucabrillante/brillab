scale_color_colorblind <- function(palette = "bright", alpha = NULL, name = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this functionality", call. = FALSE)
  }

  palette <- colorblind_palettes(palette,...)
  ggplot2::scale_color_manual(values = palette, alpha, name)
}
