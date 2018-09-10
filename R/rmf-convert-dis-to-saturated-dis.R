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
  # adjusting confining beds 
  nnlay <- dis$nlay + length(which(dis$laycbd != 0))
  cbd <- rep(0, nnlay)
  cbd[cumsum(dis$laycbd+1)[dis$laycbd != 0]] <- 1
  if(nnlay > 1) {
    thck <- botm <- dis$botm
    thck[,,1] <- dis$top - dis$botm[,,1]
    thck[,,2:nnlay] <- dis$botm[,,(2:nnlay)-1] - dis$botm[,,2:nnlay]
    dis$botm <- dis$botm[,,!cbd]
    dis$botm[,,1:(dis$nlay-1)][which(hed[,,2:(dis$nlay)] < dis$botm[,,1:(dis$nlay-1)])] <- hed[,,2:(dis$nlay)][which(hed[,,2:(dis$nlay)] < dis$botm[,,1:(dis$nlay-1)])]
    botm[,,!cbd] <- dis$botm
    botm[,,which(cbd == 1)] <- botm[,,which(cbd == 1)-1] - thck[,,which(cbd == 1)]
    dis$botm <- botm
  } 
  dis$top <- hed[,,1]
  return(dis)
}

#' @describeIn rmf_convert_dis_to_saturated_dis Deprecated function name
#' @export
convert_dis_to_saturated_dis <- function(...) {
  .Deprecated(new = "rmf_convert_dis_to_saturated_dis", old = "convert_dis_to_saturated_dis")
  rmf_convert_dis_to_saturated_dis(...)
}
