#' Create trend plots
#'
#' \code{trend_plot} creates standard line plots with transparent ribbons for showing standard
#' error of the mean to use with temporal data. Also check helpers such as \code{swp_trend}
#' for most common uses.
#'
#' TO DO: currently this function does require a block column (although this is not needed for the plot)
#' If needed include support for when block column is missing.
#' TO DO: use the y to name the axis unless y_lab is assigned
#'
#' @param df data frame or tibble that contains the data. Ideally from our lab template
#' @param y character (name) or number (index) for the column containing the variable of interest
#' @param date_col character (name) or number (index) for the column containing the Date variable. Default "Date".
#' @param block_col character (name) or number (index) for the column containing the Blocking variable. Default "Block".
#' @param treat_col character (name) or number (index) for the column containing the Treatment variable. Default "Treatment".
#' @param time_range can assume values \code{"all"} (default), when not filtering per date, or take on single
#' value indicating the starting date \code{"2020-12-31"}, or an array of two values indicating the start and end
#' \code{c("2020-12-31", "2021-01-31")}. The extremes of the range are included.
#' @param treatment_level \code{NULL} (default) or an array giving a defined order for the treatment level that would make sense for plotting
#' @param y_lab string or \code{NULL} for name of y axis in the plot
#' @param plot_title string
#' @param save \code{TRUE} or \code{FALSE} to save the file to a png
#' @param plot_width the width of the plot, default to px unless units is specified see \code{png()}
#' @param plot_height the height of the plot, default to px unless units is specified see \code{png()}
#' @param ... additional argument to be passed to the \link[grDevices]{png} function for saving the plot
#'
#'

trend_plot <- function(df,
                       y = "SWP",
                       date_col = "Date",
                       block_col = "Block",
                       treat_col = "Treatment",
                       time_range = "all",
                       treatment_level = NULL,
                       y_lab = "SWP",
                       plot_title = "My plot",
                       save = "FALSE",
                       plot_width = 1000,
                       plot_height = 651,
                       palette = colorblind_palettes("wong"),
                       ...) {
  require("dplyr")
  require("ggplot2")
  require("lubridate")

  #assign standard names to the columns of the dataframe
  #TO DO: move all this preparation stuff to an internal function to make it easy to mantain
  df <- rename(df,
               "y" = names(df[y]),
               "Date" = names(df[date_col]),
               "Block" = names(df[block_col]),
               "Treatment" = names(df[treat_col]))

  if (time_range[1] != "all") {
    if (length(time_range == 1)) {
      df <- df[df$Date >= time_range[1], ]
    } else {
      df <- df[df$Date >= time_range[1] & df$Date <= time_range[2], ]
    }
  }

  if (!is.null(treatment_level)) {
    df$Treatment <- factor(df$Treatment, levels = treatment_level)
  } else {
    df$Treatment <- factor(df$Treatment)
  }

  if (is.null(palette)) stop(simpleError(paste0("Provide a palette with at least ",
                                           length(levels(df$Treatment)), "colors")))

  df <- df %>%
    group_by(Date, Treatment) %>%
    summarise(y_mean = mean(y, na.rm = TRUE), y_se = se(y))

  df$y_min <- df$y_mean - df$y_se
  df$y_max <- df$y_mean + df$y_se
  df$year <- year(df$Date)

  p <- ggplot(df, aes(Date, y_mean)) +
    geom_ribbon(aes(ymin = y_min, ymax = y_max, fill = Treatment), alpha = 0.15) +
    geom_line(aes(color = Treatment), size = 2) +
    geom_point(aes(shape = Treatment, color = Treatment, fill = Treatment), size = 5) +
    ggtitle(plot_title) +
    ylab(y_lab) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    theme_bw() +
    theme(text = element_text(size = 24))

  if (length(unique(df$year)) > 1) p <- p + facet_wrap(df$year, scales = "free_x")

  if (save) png(width = plot_width, height = plot_height, antialias = "cleartype", ...)

  return(p)
}



