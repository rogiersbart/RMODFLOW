#' Convert a huf object to an rmf_3d_array with the number of numerical layers per hydrogeological unit
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @param bas bas object, corresponding to the huf object
#' @return nlay rmf_3d_array
#' @export
rmf_convert_huf_to_nlay <- function(huf, dis, bas) {
  nlay <- huf$top * 0
  huf_coordinates <- rmf_cell_coordinates(huf, dis = dis, include_faces = TRUE)
  dis_coordinates <- rmf_cell_coordinates(dis, include_faces = TRUE)
  ibound <- abs(bas$ibound)
  for(i in 1:huf$nhuf) {
    for(j in 1:dis$nlay) {
      nlay[,,i] <- nlay[,,i] + (!(dis_coordinates$upper[,,j] < huf_coordinates$lower[,,i] | dis_coordinates$lower[,,j] > huf_coordinates$upper[,,i])) * ibound[,,j]
    }
  }
  return(nlay)
}

#' @describeIn rmf_convert_huf_to_nlay Deprecated function name
#' @export
convert_huf_to_nlay <- function(...) {
  .Deprecated(new = "rmf_convert_huf_to_nlay", old = "convert_huf_to_nlay")
  rmf_convert_huf_to_nlay(...)
}
