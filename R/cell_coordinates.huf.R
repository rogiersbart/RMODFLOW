#' Get cell coordinates from a huf object
#' 
#' @param huf huf object
#' @return 3d array with cell coordinates
#'
#' @rdname cell_coordinates
#' @method cell_coordinates huf
#' @export
cell_coordinates.huf <- function(huf, dis = NULL)
{
  cell_coordinates <- NULL
  cell_coordinates$z <- huf$top - huf$thck/2
  class(cell_coordinates$z) <- 'rmodflow_3d_array'
  if(!is.null(dis)) {
    cell_coordinates$x <- cell_coordinates$z*0
    cell_coordinates$y <- cell_coordinates$z*0
    cell_coordinates$y[,,] <- rev(cumsum(rev(dis$delc))-rev(dis$delc)/2)
    cell_coordinates$x[,,] <- rep(c(cumsum(dis$delr)-dis$delr/2),each=dis$nrow)
  }
  return(cell_coordinates)
}
