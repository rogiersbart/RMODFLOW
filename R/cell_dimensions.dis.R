#' Get cell dimensions from a dis object
#' 
#' @param dis dis object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @param include_volume logical; should the cell volumes be included?
#' @param include_faces logical; should face areas be included?
#' @return list with cell dimension 3d arrays
#' @rdname cell_dimensions
#' @method cell_dimensions dis
#' @export
cell_dimensions.dis <- function(dis,
                                hed = NULL,
                                include_volume = FALSE,
                                include_faces = FALSE) {
  cell_dimensions <- list()
  cell_top <- dis$botm
  cell_top[,,1] <- dis$top
  cell_top[,,2:dis$nlay] <- dis$botm[,,c(1:(dis$nlay-1))]
  if(!is.null(hed)) {
    cell_top[which(cell_top > hed)] <- hed[which(cell_top > hed)] # adapt for transient hed objects!
  }
  cell_dimensions$z <- create_rmodflow_array(cell_top - dis$botm)
  cell_dimensions$x <- create_rmodflow_array(array(rep(dis$delc,dis$ncol*dis$nlay),dim=c(dis$nrow,dis$ncol,dis$nlay)))
  cell_dimensions$y <- create_rmodflow_array(array(rep(dis$delr, dis$nlay, each = dis$nrow),dim=c(dis$nrow,dis$ncol,dis$nlay)))
  if(include_volume) cell_dimensions$volume <- create_rmodflow_array(with(cell_dimensions, x * y * z))
  if(include_faces) {
    cell_dimensions$front <- create_rmodflow_array(with(cell_dimensions, x * z))
    cell_dimensions$back <- cell_dimensions$front
    cell_dimensions$left <- create_rmodflow_array(with(cell_dimensions, y * z))
    cell_dimensions$right <- cell_dimensions$left
    cell_dimensions$lower <- create_rmodflow_array(with(cell_dimensions, x * y))
    cell_dimensions$upper <- cell_dimensions$lower
  }  
  return(cell_dimensions)
}
