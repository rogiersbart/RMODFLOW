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
