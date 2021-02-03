#' Read stem water potential data from lab template
#'
#' @param path a string giving the full path to the file, make sure to replace "\" with "/" if in windows.
#' @param ... additional arguments to be given to \code{read_xlsx()}
#' @return a tibble
#'
read_swp <- function(path, ...) {
  require("readxl")
  read_xlsx(path,
            sheet = "S_SWP", na = "NA", ...)
}
