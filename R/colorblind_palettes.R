#Paul Tol color schemes
#https://personal.sron.nl/~pault/data/colourschemes.pdf
#Bang Wong
#https://www.nature.com/articles/nmeth.1618
#ibm: https://lospec.com/palette-list/ibm-color-blind-safe

colorblind_palettes <- function(palette = c("tol_bright", "tol_high_contrast",
                                            "tol_vibrant", "tol_muted", "tol_pale",
                                            "tol_dark", "tol_light", "wong", "ibm"),
                                n_colors = "all",
                                reverse = FALSE, include_gray = FALSE, include_black = FALSE) {
  #TO DO: in include_gray an include_black instead of matching by the index match by %in%

  palette <- match.arg(palette)
  if (palette == "tol_bright") colorblind_pal <- c("#4477AA", "#66CCEE", "#228833","#CCBB44", "#EE6677", "#AA3377",
                                                   "#BBBBBB")
  if (palette == "tol_high_contrast") colorblind_pal <- (c("#FFFFFF", "#DDAA33", "#BB5566", "#004488",
                                                           "#000000"))
  if (palette == "tol_vibrant") colorblind_pal <- c("#0077BB", "#33BBEE", "#009988", "#EE7733",
                                                    "#CC3311", "#EE3377", "#BBBBBB")
  if (palette == "tol_muted") colorblind_pal <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
                                                  "#DDCC77", "#CC6677", "#882255", "#AA4499")
  if (palette == "tol_pale") colorblind_pal <- c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC", "#DDDDDD")
  if (palette == "tol_dark") colorblind_pal <- c("#222255", "#225555", "#225522", "#666633", "#663333",
                                                 "#555555")
  if (palette == "tol_light") colorblind_pal <- c("#77AADD", "#99DDFF", "#44BB99", "#BBCC33", "#AAAA00",
                                                  "#EEDD88", "#EE8866", "#FFAABB", "#DDDDDD")
  if (palette == "wong") colorblind_pal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                                             "#0072B2", "#D55E00", "#CC79A7")
  if (palette == "ibm") colorblind_pal <- c("#000000", "#ffb000", "#fe6100", "#dc267f", "#785ef0", "#648fff")


  if (include_gray == FALSE) {
    if (palette == "bright" | palette == "vibrant" | palette == "muted" |
        palette == "pale" | palette == "dark" | palette == "light") {

      colorblind_pal <- colorblind_pal[1:length(palette) - 1]
    }
  } else {
    colorblind_pal
  }

  #black looks good when black is not used in the plot for a different purpose
  #for example in error bars
  if (include_black == FALSE) {
    if (palette == "ibm" | palette == "wong") {
      colorblind_pal <- colorblind_pal[2:length(colorblind_pal)]
    }
  } else {
    colorblind_pal
  }

  if (n_colors == "all") n_colors <- length(colorblind_pal)

  if (reverse == TRUE) colorblind_pal[round(seq(length(colorblind_pal), 1, length.out = n_colors))]

  colorblind_pal[round(seq(1, length(colorblind_pal), length.out = n_colors))]
}
