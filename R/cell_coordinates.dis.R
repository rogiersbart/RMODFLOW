#' Get cell x, y and z coordinates from a dis object
#' 
#' @param dis dis object
#' @return list with with cell coordinate 3d arrays
#'
#' @rdname cell_coordinates
#' @method cell_coordinates dis
#' @export
cell_coordinates.dis <- function(dis)
{
  cell_coordinates <- NULL
  cell_coordinates$z <- dis$BOTM*NA
  cell_coordinates$z[,,1] <- (dis$TOP+dis$BOTM[,,1])/2
  for(k in 2:dis$NLAY) {
    cell_coordinates$z[,,k] <- (dis$BOTM[,,(k-1)]+dis$BOTM[,,k])/2
  }
  class(cell_coordinates$z) <- 'modflow_3d_array'
  cell_coordinates$x <- cell_coordinates$z*0
  cell_coordinates$y <- cell_coordinates$z*0
  cell_coordinates$y[,,] <- rev(cumsum(rev(dis$DELC))-rev(dis$DELC)/2)
  cell_coordinates$x[,,] <- rep(c(cumsum(dis$DELR)-dis$DELR/2),each=dis$NROW)
  return(cell_coordinates)
}
