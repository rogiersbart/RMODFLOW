#' Convert a dis object to correspond to the saturated volume
#' 
#' @param dis dis object
#' @param hed hed object
#' @return dis object
#' @export
convert_dis_to_saturated_dis <- function(dis, hed) {
  dis$BOTM[,,1:(dis$NLAY-1)][which(hed[,,2:(dis$NLAY)] < dis$BOTM[,,1:(dis$NLAY-1)])] <- hed[,,2:(dis$NLAY)][which(hed[,,2:(dis$NLAY)] < dis$BOTM[,,1:(dis$NLAY-1)])]
  dis$TOP <- hed[,,1]
  return(dis)
}
