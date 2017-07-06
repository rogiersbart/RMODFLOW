#' Get cell dimensions from a huf object
#' 
#' @param huf huf object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @return list with cell dimension 3d arrays
#'
#' @rdname rmf_cell_dimensions
#' @method rmf_cell_dimensions huf
#' @export
rmf_cell_dimensions.huf <- function(huf,
                                    dis = NULL,
                                    hed = NULL,
                                    include_volume = FALSE,
                                    include_faces = FALSE) {
  cell_dimensions <- list()
  huf_top <- huf$top
  for(k in 2:huf$nhuf) huf_top[,,k] <- huf_top[,,(k-1)] - huf$thck[,,k-1]
  huf_thickness <- huf$thck
  huf_bottom <- huf_top
  huf_bottom[,,1:(huf$nhuf-1)] <- huf_top[,,2:huf$nhuf]
  huf_bottom[,,huf$nhuf] <- huf_top[,,huf$nhuf] - huf_thickness[,,huf$nhuf]
  if(!is.null(hed)) {
    huf_top[which(huf_top > hed)] <- hed[which(huf_top > hed)]
  }
  cell_dimensions$z <- huf_top - huf_bottom
  if(!is.null(dis)) {
    cell_dimensions$x <- cell_dimensions$y <- cell_dimensions$z
    dis_cell_dimensions <- cell_dimensions(dis)
    cell_dimensions$x[,,] <- dis_cell_dimensions$x[,,1]
    cell_dimensions$y[,,] <- dis_cell_dimensions$y[,,1]
  }
  if(include_volume) cell_dimensions$volume <- structure(with(cell_dimensions, x * y * z), class = 'rmf_3d_array')
  if(include_faces) {
    cell_dimensions$front <- structure(with(cell_dimensions, x * z), class = 'rmf_3d_array')
    cell_dimensions$back <- cell_dimensions$front
    cell_dimensions$left <- structure(with(cell_dimensions, y * z), class = 'rmf_3d_array')
    cell_dimensions$right <- cell_dimensions$left
    cell_dimensions$lower <- structure(with(cell_dimensions, x * y), class = 'rmf_3d_array')
    cell_dimensions$upper <- cell_dimensions$lower
  }  
  return(cell_dimensions)
}
