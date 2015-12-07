#' Get information from a dis object at a certain grid cell
#' 
#' @param dis a discretization file object
#' @param i row number
#' @param j column number
#' @return \code{NULL}
#'
#' @rdname cell_info
#' @method cell_info dis
#' @export
cell_info.dis <- function(dis, i, j)
{
  cat('Column width = ',dis$delr[j], '\n')
  cat('Row width = ', dis$delc[i], '\n')
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  cat('\t\t Top \t\t Bottom \t Thickness\n', sep='')
  cat('Layer 1:\t', dis$top[i, j], '\t', dis$botm[i,j,1],'\t', dis$top[i, j]-dis$botm[i,j,1],'\n', sep='')
  
  for(i in 2:dis$nlay)
  {
    cat('Layer ',i,':\t', dis$botm[i, j, i-1], '\t', dis$botm[i,j,i],'\t', dis$botm[i, j, i-1]-dis$botm[i,j,i],'\n', sep='')
  }
}
