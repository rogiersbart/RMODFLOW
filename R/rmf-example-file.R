#' Path to example file
#'
#' @param filename filename of the example file. If NULL (default), returns vector of all example file filenames.
#'
#' @return path to example file
#' @export
#'
#' @examples
rmf_example_file <- function(filename = NULL) {
  if(is.null(filename)) {
    dir(system.file("extdata", package = "RMODFLOW"))
  } else {
    filename <- system.file(paste0("extdata/", filename), package = "RMODFLOW")
    if(filename == "") {
      stop("Example file not found. Please check the list of example files with rmf_example_file().")
    } else {
      return(filename)
    }
  }
}
