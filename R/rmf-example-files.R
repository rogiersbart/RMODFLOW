#' Path to example files
#'
#' @param pattern pattern to match example file filenames. If NULL (default), returns vector of all example file filenames.
#'
#' @return path to example files
#' @export
#'
#' @examples
rmf_example_files <- function(pattern = NULL) {
  example_files <- dir(system.file("extdata", package = "RMODFLOW"))
  if(is.null(pattern)) {
    return(example_files)
  } else {
    filenames <- grep(pattern, example_files, value = TRUE)
    if(length(filenames) == 0) {
      stop("Example files not found given the provided pattern. Please check the list of example files with rmf_example_files().")
    } else {
      return(filenames)
    }
  }
}
