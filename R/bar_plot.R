#' Create bar plots
#'
#'\code{trend_plot} creates standard bar plots with standard error of the mean bars.
#'Multiple panels (facets) can be produced with the \code{group_col} argument
#'
#' @param df data frame or tibble that contains the data. Ideally from our lab template
#' @param y character (name) or number (index) for the column containing the variable of interest
#' @param time_range can assume values NULL (default), when not filtering per date. or
#' take one single value "2020-12-31", or an array of two values indicating the start and end
#' c("2020-12-31", "2021-01-31"). The extremes of the range are included. Use only when not the group_col
#' is a date column
#' @param group_col character (name) or number (index) for the column containing the faceting
#' variable used to split the plots in different panels
#' @param block_col character (name) or number (index) for the column containing the Blocking variable. Default "Block".
#' @param treat_col character (name) or number (index) for the column containing the Treatment variable. Default "Treatment".
#' @param treatment_level \code{NULL} (default) or an array giving a defined order for the treatment level that would make sense for plotting
#' @param y_lab string or \code{NULL} for name of y axis in the plot
#' @param plot_title string
#' @param save \code{TRUE} or \code{FALSE} to save the file to a png
#' @param plot_width the width of the plot, default to px unless units is specified see \code{png()}
#' @param plot_height the height of the plot, default to px unless units is specified see \code{png()}
#' @param ... additional argument to be passed to the \link[grDevices]{png} function for saving the plot
#'
#' @return a ggplot object
#'
bar_plot <- function(df,
                     y = "Time",
                     time_range = NULL,
                     group_col = "Date",
                     block_col = "Block",
                     treat_col = "Treatment",
                     treatment_level = NULL,
                     y_lab = NULL,
                     plot_title = "My plot",
                     save = "FALSE",
                     plot_width = 1000,
                     plot_height = 651,
                     palette = colorblind_palettes("wong"),
                     ...) {
  #TO DO: this function does not accept when group_col (but also block_col) has a single value!
  #needs to be changed to include the possibility of having null values there (change at the rename step)

  #TO DO: can move the plot saving out from this function in order to use the dots
  #to further customize the graphics

  require("dplyr")
  require("ggplot2")
  df <- rename(df,
               "y" = names(df[y]),
               "Group" = names(df[group_col]),
               "Block" = names(df[block_col]),
               "Treatment" = names(df[treat_col]))
  if (is.null(y_lab)) y_lab <- y

  if (!is.null(time_range)) {
    warning("using the grouping column as a date column for selecting time range")
    if (length(time_range) == 1) {
      df <- df[df$Group == time_range[1], ]
    } else {
      df <- df[df$Group >= time_range[1] & df$Group <= time_range[2], ]
    }
  }

  if (!is.null(treatment_level)) {
    df$Treatment <- factor(df$Treatment, levels = treatment_level)
    } else {
    df$Treatment <- factor(df$Treatment)
    }
  df$Group <- factor(df$Group)

  if (is.null(palette)) stop(simpleError(paste0("Provide a palette with at least ",
                                                length(levels(df$Treatment)), "colors")))

  df <- df %>%
    group_by(Group, Treatment) %>%
    summarise(y_mean = mean(y, na.rm = TRUE), y_se = se(y))

  df$y_min <- df$y_mean - df$y_se
  df$y_max <- df$y_mean + df$y_se

  p <- ggplot(df, aes(Treatment, y_mean)) +
    geom_col(aes(fill = Treatment), color = "black", size = 1.25) +
    facet_wrap(vars(Group)) +
    geom_errorbar(aes(ymin = y_min, ymax = y_max, group = Treatment), width = 0.25, size = 1.25) +
    ggtitle(plot_title) +
    ylab(y_lab) +
    scale_fill_manual(values = palette) +
    theme_bw() +
    theme(text = element_text(size = 24))

  if (length(unique(df$Group)) > 1) p <- p + facet_wrap(df$Group)

  if (save) png(width = plot_width, height = plot_height, antialias = "cleartype", ...)

  return(p)
}
