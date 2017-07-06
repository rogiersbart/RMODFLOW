#' Convert a dis object to correspond to the saturated volume
#' 
#' @param dis dis object
#' @param hed hed object
#' @return dis object
#' @export
rmf_convert_dis_to_saturated_dis <- function(dis,
                                             hed, 
                                             l = NULL) {
  if(length(dim(hed))==4) {
    if(!is.null(l)) {
      hed <- hed[,,,l]
    } else {
      warning('Using final stress period heads to determine saturated part of grid.', call. = FALSE)
      hed <- hed[,,,dim(hed)[4]]
    }
  }
  dis$botm[,,1:(dis$nlay-1)][which(hed[,,2:(dis$nlay)] < dis$botm[,,1:(dis$nlay-1)])] <- hed[,,2:(dis$nlay)][which(hed[,,2:(dis$nlay)] < dis$botm[,,1:(dis$nlay-1)])]
  dis$top <- hed[,,1]
  return(dis)
}

#' @describeIn rmf_convert_dis_to_saturated_dis Deprecated function name
convert_dis_to_saturated_dis <- function(...) {
  .Deprecated(new = "rmf_convert_dis_to_saturated_dis", old = "convert_dis_to_saturated_dis")
  rmf_convert_dis_to_saturated_dis(...)
}
