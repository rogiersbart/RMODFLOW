#' Convert an \code{ibound} array to lower, upper, left, right, front and back logical arrays indicating presence of a neighbouring active cell
#' 
#' @param ibound modflow basic file \code{ibound} array
#' @return list of lower, upper, left, right, front and back logical 3d arrays
#' @export
rmf_convert_ibound_to_neighbours <- function(ibound) {
  ibound <- ibound != 0
  nrow <- dim(ibound)[1]
  ncol <- dim(ibound)[2]
  nlay <- dim(ibound)[3]
  neighbours <- list()
  neighbours$lower <- array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$upper <- array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$left <- array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$right <- array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$front <- array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$back <- array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$lower[,,1:(nlay-1)] <- ibound[,,2:nlay]
  neighbours$upper[,,2:nlay] <- ibound[,,1:(nlay-1)]
  neighbours$left[,2:ncol,] <- ibound[,1:(ncol-1),]
  neighbours$right[,1:(ncol-1),] <- ibound[,2:ncol,]
  neighbours$front[1:(nrow-1),,] <- ibound[2:nrow,,]
  neighbours$back[2:nrow,,] <- ibound[1:(nrow-1),,]
  return(neighbours)
}

#' @describeIn rmf_convert_ibound_to_neighbours Deprecated function name
convert_ibound_to_neighbours <- function(...) {
  .Deprecated(new = "rmf_convert_ibound_to_neighbours", old = "convert_ibound_to_neighbours")
  rmf_convert_ibound_to_neighbours(...)
}
