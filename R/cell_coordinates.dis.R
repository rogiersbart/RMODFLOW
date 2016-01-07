#' Get cell x, y and z coordinates from a dis object
#' 
#' @param dis dis object
#' @return list with with cell coordinate 3d arrays
#'
#' @rdname cell_coordinates
#' @method cell_coordinates dis
#' @export
cell_coordinates.dis <- function(dis, include_faces = FALSE)
{
  cell_coordinates <- NULL
  cell_coordinates$z <- dis$botm*NA
  cell_coordinates$z[,,1] <- (dis$top+dis$botm[,,1])/2
  for(k in 2:dis$nlay) {
    cell_coordinates$z[,,k] <- (dis$botm[,,(k-1)]+dis$botm[,,k])/2
  }
  class(cell_coordinates$z) <- 'rmodflow_3d_array'
  cell_coordinates$x <- cell_coordinates$z*0
  cell_coordinates$y <- cell_coordinates$z*0
  cell_coordinates$y[,,] <- rev(cumsum(rev(dis$delc))-rev(dis$delc)/2)
  cell_coordinates$x[,,] <- rep(c(cumsum(dis$delr)-dis$delr/2),each=dis$nrow)
  if(include_faces) {
    cell_coordinates$lower <- dis$botm
    cell_coordinates$upper <- 2 * cell_coordinates$z - dis$botm
    cell_coordinates$left <- cell_coordinates$x - dis$delr/2
    cell_coordinates$right <- cell_coordinates$x + dis$delr/2 
    cell_coordinates$front <- cell_coordinates$y - rev(dis$delc)/2
    cell_coordinates$back <- cell_coordinates$y + rev(dis$delc)/2
  }
  return(cell_coordinates)
}
