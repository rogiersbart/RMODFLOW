#' Get cell center z coordinates from a dis object
#' 
#' @param dis dis object
#' @return 3d array with cell center z coordinates
#'
#' @rdname cell_centers
#' @method cell_centers dis
#' @export
cell_centers.dis <- function(dis)
{
  cell_centers <- dis$BOTM*NA
  cell_centers[,,1] <- (dis$TOP+dis$BOTM[,,1])/2
  for(k in 2:dis$NLAY)
  {
    cell_centers[,,k] <- (dis$BOTM[,,(k-1)]+dis$BOTM[,,k])/2
  }
  return(cell_centers)
}