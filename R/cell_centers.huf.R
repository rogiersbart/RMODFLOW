#' Get cell center z coordinates from a huf object
#' 
#' @param huf huf object
#' @return 3d array with cell center z coordinates
#'
#' @rdname cell_centers
#' @method cell_centers huf
#' @export
cell_centers.huf <- function(huf, dis = NULL)
{
  cell_centers <- NULL
  cell_centers$z <- huf$TOP - huf$THCK/2
  class(cell_centers$z) <- 'modflow_3d_array'
  if(!is.null(dis)) {
    cell_centers$x <- cell_centers$z*0
    cell_centers$y <- cell_centers$z*0
    cell_centers$y[,,] <- rev(cumsum(rev(dis$DELC))-rev(dis$DELC)/2)
    cell_centers$x[,,] <- rep(c(cumsum(dis$DELR)-dis$DELR/2),each=dis$NROW)
  }
  return(cell_centers)
}
