#' Path to example file
#'
#' @param filename filename of the example file
#'
#' @return path to example file
#' @export
#'
#' @examples
rmf_example_file <- function(filename = NULL) {
  filename <- system.file(paste0("extdata/", filename), package = "RMODFLOW")
  if(filename[1] == "") {
    stop("Example file not found. Please check the list of example files with rmf_example_files().")
  } else {
    return(filename)
  }
}
