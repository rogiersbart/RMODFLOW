#' Generic function to get information at a certain grid node
#' 
#' @rdname node_info
#' @export
node_info <- function(...)
{
  UseMethod('node_info')
}