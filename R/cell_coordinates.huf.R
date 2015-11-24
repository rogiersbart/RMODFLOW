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
  cell_coordinates$z <- huf$TOP - huf$THCK/2
  class(cell_coordinates$z) <- 'modflow_3d_array'
  if(!is.null(dis)) {
    cell_coordinates$x <- cell_coordinates$z*0
    cell_coordinates$y <- cell_coordinates$z*0
    cell_coordinates$y[,,] <- rev(cumsum(rev(dis$DELC))-rev(dis$DELC)/2)
    cell_coordinates$x[,,] <- rep(c(cumsum(dis$DELR)-dis$DELR/2),each=dis$NROW)
  }
  return(cell_coordinates)
}
