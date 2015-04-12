#' Generic function to get model performance measures
#' 
#' @rdname performance
#' @export
performance <- function(...)
{
  UseMethod('performance')
}