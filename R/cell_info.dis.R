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
  cat('Column width = ',dis$DELR[j], '\n')
  cat('Row width = ', dis$DELC[i], '\n')
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  cat('\t\t Top \t\t Bottom \t Thickness\n', sep='')
  cat('Layer 1:\t', dis$TOP[i, j], '\t', dis$BOTM[i,j,1],'\t', dis$TOP[i, j]-dis$BOTM[i,j,1],'\n', sep='')
  
  for(i in 2:dis$NLAY)
  {
    cat('Layer ',i,':\t', dis$BOTM[i, j, i-1], '\t', dis$BOTM[i,j,i],'\t', dis$BOTM[i, j, i-1]-dis$BOTM[i,j,i],'\n', sep='')
  }
}
