#' Convert a huf to a mask object
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @param bas bas object, corresponding to the huf object
#' @return mask rmf_3d_array
#' @export
rmf_convert_huf_to_mask <- function(huf, dis, bas) {
  mask <- rmf_convert_huf_to_nlay(huf = huf, dis = dis, bas = bas)
  mask[which(mask==0)] <- NA
  mask <- mask/mask
  mask[which(huf$thck==0)] <- NA
  return(mask)
}

#' @describeIn rmf_convert_huf_to_mask Deprecated function name
convert_huf_to_mask <- function(...) {
  .Deprecated(new = "rmf_convert_huf_to_mask", old = "convert_huf_to_mask")
  rmf_convert_huf_to_mask(...)
}

