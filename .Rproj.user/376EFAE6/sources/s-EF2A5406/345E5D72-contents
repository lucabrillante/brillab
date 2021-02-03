#' Plot stem water potential data
#'
#' A wrapper for trend_plot for the common case of plotting stem water potentials
#'
#' @param df a tibble or data frame as the one coming from the function read_swp
#' @param y character (name) or number (index) for the column containing the variable of interest. Default "SWP", the name of the column in the lab template.
#' @param ... additional argumnets to be passed to \link[brillab]{trend_plot}()
#'
#' @return a ggplot object
swp_trend <- function(df, y = "SWP", ...){
  df[[y]] <- df[[y]]* -0.1
  trend_plot(df, y = y, y_lab = "Stem Water Potential (MPa)", ...)
}
