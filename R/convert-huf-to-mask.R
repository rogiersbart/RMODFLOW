#' Convert a huf to a mask object
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @param bas bas object, corresponding to the huf object
#' @return mask rmodflow_3d_array
#' @export
convert_huf_to_mask <- function(huf, dis, bas) {
  mask <- RMODFLOW::convert_huf_to_nlay(huf = huf, dis = dis, bas = bas)
  mask[which(mask==0)] <- NA
  mask <- mask/mask
  return(mask)
}
