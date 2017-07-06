#' Convert a huf to a dis object (for plotting)
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @return dis object containing the layers of the huf object
#' @export
rmf_convert_huf_to_dis <- function(huf,
                                   dis) {
  new_dis <- rmf_create_dis(nlay = huf$nhuf,
                            nrow = dis$nrow,
                            ncol = dis$ncol,
                            delr = dis$delr,
                            delc = dis$delc,
                            top = huf$top[,,1],
                            botm = huf$top - huf$thck)
  new_dis$top[which(new_dis$top > dis$top)] <- dis$top[which(new_dis$top > dis$top)]
  for(k in 1:huf$nhuf) {
    new_dis$botm[,,k][which(new_dis$botm[,,k] > dis$top)] <- dis$top[which(new_dis$botm[,,k] > dis$top)]
    new_dis$botm[,,k][which(new_dis$botm[,,k] < dis$botm[,,dis$nlay])] <- dis$botm[,,dis$nlay][which(new_dis$botm[,,k] < dis$botm[,,dis$nlay])]
  }
  new_dis$botm <- rmf_create_array(new_dis$botm)
  new_dis$top <- rmf_create_array(new_dis$top)
  return(new_dis)
}

#' @describeIn rmf_convert_huf_to_dis Deprecated function name
convert_huf_to_dis <- function(...) {
  .Deprecated(new = "rmf_convert_huf_to_dis", old = "convert_huf_to_dis")
  rmf_convert_huf_to_dis(...)
}
