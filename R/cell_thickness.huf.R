#' Get cell thicknesses from a huf object
#' 
#' @param huf huf object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @return 3d array with cell thicknesses
#'
#' @rdname cell_thickness
#' @method cell_thickness huf
#' @export
cell_thickness.huf <- function(huf, hed = NULL)
{
  cell_thickness <- huf$THCK
  if(!is.null(hed)) {
    huf$TOP[which(huf$TOP > hed)] <- hed[which(huf$TOP > hed)]
  }
  return(cell_centers)
}
