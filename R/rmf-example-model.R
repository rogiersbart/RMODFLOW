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
