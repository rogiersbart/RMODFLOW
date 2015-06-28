#' Get cell center z coordinates from a huf object
#' 
#' @param huf huf object
#' @return 3d array with cell center z coordinates
#'
#' @rdname cell_centers
#' @method cell_centers huf
#' @export
cell_centers.huf <- function(huf)
{
  cell_centers <- huf$TOP - huf$THCK/2
  return(cell_centers)
}