#' Get cell dimensions from a dis object
#' 
#' @param dis dis object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @return list with cell dimension 3d arrays
#'
#' @rdname cell_dimensions
#' @method cell_dimensions dis
#' @export
cell_dimensions.dis <- function(dis, hed = NULL, include_volume = FALSE, include_faces = FALSE)
{
  cell_dimensions <- list()
  cell_top <- dis$BOTM
  cell_top[,,1] <- dis$TOP
  cell_top[,,2:dis$NLAY] <- dis$BOTM[,,c(1:(dis$NLAY-1))]
  if(!is.null(hed)) {
    cell_top[which(cell_top > hed)] <- hed[which(cell_top > hed)] # adapt for transient hed objects!
  }
  cell_dimensions$z <- structure(cell_top - dis$BOTM, class = '3d_array')
  cell_dimensions$x <- structure(array(rep(dis$DELC,dis$NCOL*dis$NLAY),dim=c(dis$NROW,dis$NCOL,dis$NLAY)), class = '3d_array')
  cell_dimensions$y <- structure(array(rep(dis$DELR, dis$NLAY, each = dis$NROW),dim=c(dis$NROW,dis$NCOL,dis$NLAY)), class = '3d_array')
  if(include_volume) cell_dimensions$volume <- structure(with(cell_dimensions, x * y * z), class = '3d_array')
  if(include_faces) {
    cell_dimensions$front <- structure(with(cell_dimensions, x * z), class = '3d_array')
    cell_dimensions$back <- cell_dimensions$front
    cell_dimensions$left <- structure(with(cell_dimensions, y * z), class = '3d_array')
    cell_dimensions$right <- cell_dimensions$left
    cell_dimensions$lower <- structure(with(cell_dimensions, x * y), class = '3d_array')
    cell_dimensions$upper <- cell_dimensions$lower
  }  
  return(cell_dimensions)
}
