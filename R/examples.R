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

#' List example model files
#'
#' @param model
#'
#' @return example model files
#' @export
#'
#' @examples
rmf_example_model <- function(model) {
  rmf_example_files(model) %>% 
    rmf_example_file()
}

#' List example models
#'
#' @return example model names
#' @export
#'
#' @examples
rmf_example_models <- function() {
  example_files <- dir(system.file("extdata", package = "RMODFLOW"))
  example_files <- gsub(".nam", "", example_files[grep(".nam", example_files)])
  return(example_files)
}
