#' Standard Error of the Mean
#'
#' Calculate the standard error of the mean. Avoid the need for calling a package just for it.
#'
#' @param x a numeric vector
#' @param na.rm TRUE or FALSE, default to TRUE
se <- function(x, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}
