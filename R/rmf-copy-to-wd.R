#' Copy files from specified paths to current working directory
#'
#' @param filenames character vector of filenames
#' @param ... additional arguments provided to file.copy
#'
#' @export
rmf_copy_to_wd <- function(filenames, ...) {
  file.copy(filenames, basename(filenames), ...)
}