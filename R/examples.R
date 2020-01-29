#' Path to example file
#'
#' @param filename filename of the example file
#'
#' @return path to example file
#' @export
#'
#' @examples
rmf_example_file <- function(filename = NULL) {
  if (is.null(filename)) {
    message("These are the available files:")
    rmf_example_files() %>%
      return()
  } else {
    filename <- system.file(paste0("extdata/", filename), package = "RMODFLOW")
    if(filename[1] == "") {
      stop("Example file not found. Please check the list of example files with rmf_example_files().")
    } else {
      return(filename)
    }
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
rmf_example_model <- function(model = NULL) {
  if (is.null(model)) {
    message("These are the available models:")
    rmf_example_models() %>%
      return()
  } else {
    rmf_example_files(model) %>% 
      rmf_example_file() %>%
      return()
  }
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

#' Access example MODFLOW files
#'
#' @param name filename or model name; if NULL provides a list of possibilities
#' @param code identification of the MODFLOW variant, only required for the
#'   examples that come with the code archives available online
#'
#' @return character vector of model or file names/paths
#' @export
#'
#' @examples
rmf_example <- function(name = NULL, code = NULL) {
  # TODO extend with access to cfp, lgr and nwt examples
  # TODO drastically simplify API; this function should be sufficient for the 
  #      user if you ask me
  if (is.null(name) & is.null(code)) {
    return(rmf_example_models())
  }
  if (is.null(code)) {
    if (name %in% rmf_example_models()) return(rmf_example_model(name))
    if (name %in% rmf_example_files()) return(rmf_example_file(name))
  }
  if (grepl("2005", code)) {
    example_files <- fs::dir_ls(paste0(getOption("RMODFLOW.path"),
                                       "/MODFLOW-2005"), regexp = "MODFLOW-2005/test-",
                                type = "file",
                                recurse = TRUE)
    if (is.null(name)) {
      example_files %>%
        fs::path_file() %>%
        fs::path_ext_remove() %>%
        unique() %>% 
        return()
    } else {
      return(stringr::str_subset(example_files, name))
    }
  }
}