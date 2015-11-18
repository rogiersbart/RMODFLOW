#' Get cell thicknesses from a dis object
#' 
#' @param dis dis object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @return 3d array with cell thicknesses
#'
#' @rdname cell_thickness
#' @method cell_thickness dis
#' @export
cell_thickness.dis <- function(dis, hed = NULL)
{
  cell_top <- dis$BOTM
  cell_top[,,1] <- dis$TOP
  cell_top[,,2:dis$NLAY] <- dis$BOTM[,,c(1:(dis$NLAY-1))]
  if(!is.null(hed)) {
    cell_top[which(cell_top > hed)] <- hed[which(cell_top > hed)] # adapt for transient hed objects!
  }
  cell_thickness <- cell_top - dis$BOTM    
  class(cell_thickness) <- 'modflow_3d_array'
  return(cell_thickness)
}
