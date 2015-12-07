#' Convert a huf to a dis object (for plotting)
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @return dis object containing the layers of the huf object
#' @export
convert_huf_to_dis <- function(huf, dis) {
  new_dis <- create_dis(nlay = huf$nhuf,
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
  return(new_dis)
}
