#' Generic function to get cell dimensions
#' 
#' @rdname cell_dimensions
#' @export
cell_dimensions <- function(...)
{
  UseMethod('cell_dimensions')
}