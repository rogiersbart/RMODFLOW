#' Get cell x, y and z coordinates from a dis object
#' 
#' @param dis dis object
#' @param include_faces logical; should face coordinates be included?
#' @return list with with cell coordinate 3d arrays
#' @rdname rmf_cell_coordinates
#' @method rmf_cell_coordinates dis
#' @export
rmf_cell_coordinates.dis <- function(dis,
                                     include_faces = FALSE) {
  cell_coordinates <- NULL
  cell_coordinates$z <- dis$botm*NA
  cell_coordinates$z[,,1] <- (dis$top+dis$botm[,,1])/2
  
  nnlay <- dis$nlay + length(which(dis$laycbd != 0))
  if(nnlay > 1) {
    for(k in 2:nnlay) {
      cell_coordinates$z[,,k] <- (dis$botm[,,(k-1)]+dis$botm[,,k])/2
    }
  }
  class(cell_coordinates$z) <- 'rmf_3d_array'
  # remove the confining beds
  cbd <- rep(0, nnlay)
  cbd[cumsum(dis$laycbd+1)[dis$laycbd != 0]] <- 1
  cell_coordinates$z <- cell_coordinates$z[,,!cbd]
  
  cell_coordinates$x <- cell_coordinates$z*0
  cell_coordinates$y <- cell_coordinates$z*0
  cell_coordinates$y[,,] <- rev(cumsum(rev(dis$delc))-rev(dis$delc)/2)
  cell_coordinates$x[,,] <- rep(c(cumsum(dis$delr)-dis$delr/2),each=dis$nrow)
  if(include_faces) {
    dis$delr <- array(rep(dis$delr,each=dis$nrow),dim=c(dis$nrow,dis$ncol,dis$nlay))
    dis$delc <- array(rep(dis$delc,dis$ncol),dim=c(dis$nrow,dis$ncol,dis$nlay))
    cell_coordinates$lower <- dis$botm[,,!cbd]
    cell_coordinates$upper <- 2 * cell_coordinates$z - dis$botm[,,!cbd]
    cell_coordinates$left <- cell_coordinates$x - dis$delr/2
    cell_coordinates$right <- cell_coordinates$x + dis$delr/2 
    cell_coordinates$front <- cell_coordinates$y - dis$delc/2
    cell_coordinates$back <- cell_coordinates$y + dis$delc/2
  }
  return(cell_coordinates)
}
