#' Convert a huf object to an rmodflow_3d_array with the number of numerical layers per hydrogeological unit
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @param bas bas object, corresponding to the huf object
#' @return nlay rmodflow_3d_array
#' @export
convert_huf_to_nlay <- function(huf, dis, bas) {
  nlay <- huf$top * 0
  huf_coordinates <- RMODFLOW::cell_coordinates(huf, dis = dis, include_faces = TRUE)
  dis_coordinates <- RMODFLOW::cell_coordinates(dis, include_faces = TRUE)
  ibound <- abs(bas$ibound)
  for(i in 1:huf$nhuf) {
    for(j in 1:dis$nlay) {
      nlay[,,i] <- nlay[,,i] + (!(dis_coordinates$upper[,,j] < huf_coordinates$lower[,,i] | dis_coordinates$lower[,,j] > huf_coordinates$upper[,,i])) * ibound[,,j]
    }
  }
  return(nlay)
}
