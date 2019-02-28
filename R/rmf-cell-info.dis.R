#' Get information from a dis object at a certain grid cell
#' 
#' @param dis a discretization file object
#' @param i row number
#' @param j column number
#' @return \code{NULL}
#'
#' @rdname rmf_cell_info
#' @method rmf_cell_info dis
#' @export
rmf_cell_info.dis <- function(dis,
                              i,
                              j) {
  cat('Column width = ',dis$delr[j], '\n')
  cat('Row width = ', dis$delc[i], '\n')
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  cat('\t\t Top \t\t Bottom \t Thickness\n', sep='')
  cat('Layer 1:\t', dis$top[i, j], '\t', dis$botm[i,j,1],'\t', dis$top[i, j]-dis$botm[i,j,1],'\n', sep='')
  
  nnlay <- dis$nlay + length(which(dis$laycbd != 0))
  if(nnlay > 1) {
    cbd <- rep(0, nnlay)
    cbd[cumsum(dis$laycbd+1)[dis$laycbd != 0]] <- 1
    for(k in 2:nnlay)
    {
      if(cbd[k]) {
        cat('Quasi-3D Confining Bed below layer ',k-1,':\t', dis$botm[i, j, k-1], '\t', dis$botm[i,j,k],'\t', dis$botm[i, j, k-1]-dis$botm[i,j,k],'\n', sep='')
      } else {
        cat('Layer ',k,':\t', dis$botm[i, j, k-1], '\t', dis$botm[i,j,k],'\t', dis$botm[i, j, k-1]-dis$botm[i,j,k],'\n', sep='')
      }
    }
  }

}
