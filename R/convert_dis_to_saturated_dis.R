#' Convert a dis object to correspond to the saturated volume
#' 
#' @param dis dis object
#' @param hed hed object
#' @return dis object
#' @export
convert_dis_to_saturated_dis <- function(dis, hed) {
  dis$botm[,,1:(dis$nlay-1)][which(hed[,,2:(dis$nlay)] < dis$botm[,,1:(dis$nlay-1)])] <- hed[,,2:(dis$nlay)][which(hed[,,2:(dis$nlay)] < dis$botm[,,1:(dis$nlay-1)])]
  dis$top <- hed[,,1]
  return(dis)
}
