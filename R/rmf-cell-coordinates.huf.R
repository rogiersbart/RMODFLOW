#' Get cell coordinates from a huf object
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @param include_faces logical; should face coordinates be included?
#' @return 3d array with cell coordinates
#'
#' @rdname rmf_cell_coordinates
#' @method rmf_cell_coordinates huf
#' @export
rmf_cell_coordinates.huf <- function(huf,
                                     dis = NULL,
                                     include_faces = FALSE) {
  cell_coordinates <- NULL
  cell_coordinates$z <- huf$top - huf$thck/2
  class(cell_coordinates$z) <- 'rmf_3d_array'
  if(!is.null(dis)) {
    cell_coordinates$x <- cell_coordinates$z*0
    cell_coordinates$y <- cell_coordinates$z*0
    cell_coordinates$y[,,] <- rev(cumsum(rev(dis$delc))-rev(dis$delc)/2)
    cell_coordinates$x[,,] <- rep(c(cumsum(dis$delr)-dis$delr/2),each=dis$nrow)
  }
  if(include_faces) {
    dis$delr <- array(rep(dis$delr,each=dis$nrow),dim=c(dis$nrow,dis$ncol,huf$nhuf))
    dis$delc <- array(rep(dis$delc,dis$ncol),dim=c(dis$nrow,dis$ncol,huf$nhuf))
    cell_coordinates$lower <- cell_coordinates$z - huf$thck/2
    cell_coordinates$upper <- cell_coordinates$z + huf$thck/2
    cell_coordinates$left <- cell_coordinates$x - dis$delr/2
    cell_coordinates$right <- cell_coordinates$x + dis$delr/2 
    cell_coordinates$front <- cell_coordinates$y - dis$delc/2
    cell_coordinates$back <- cell_coordinates$y + dis$delc/2
  }
  return(cell_coordinates)
}
