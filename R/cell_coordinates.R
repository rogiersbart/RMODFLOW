#' Generic function to get cell coordinates
#' 
#' @rdname cell_centers
#' @export
cell_coordinates <- function(...)
{
  UseMethod('cell_coordinates')
}
