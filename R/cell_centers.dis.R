#' Get cell center x, y and z coordinates from a dis object
#' 
#' @param dis dis object
#' @return 3d array with cell center z coordinates
#'
#' @rdname cell_centers
#' @method cell_centers dis
#' @export
cell_centers.dis <- function(dis)
{
  cell_centers <- NULL
  cell_centers$z <- dis$BOTM*NA
  cell_centers$z[,,1] <- (dis$TOP+dis$BOTM[,,1])/2
  for(k in 2:dis$NLAY) {
    cell_centers$z[,,k] <- (dis$BOTM[,,(k-1)]+dis$BOTM[,,k])/2
  }
  class(cell_centers$z) <- 'modflow_3d_array'
  cell_centers$x <- cell_centers$z*0
  cell_centers$y <- cell_centers$z*0
  cell_centers$y[,,] <- rev(cumsum(rev(dis$DELC))-rev(dis$DELC)/2)
  cell_centers$x[,,] <- rep(c(cumsum(dis$DELR)-dis$DELR/2),each=dis$NROW)
  return(cell_centers)
}
