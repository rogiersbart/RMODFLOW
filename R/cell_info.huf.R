#' Get information from a huf object at a certain grid cell
#' 
#' @param huf a hydrogeologic unit file object
#' @param i row number
#' @param j column number
#' @return \code{NULL}
#'
#' @rdname cell_info
#' @method cell_info huf
#' @export
cell_info.huf <- function(huf, i, j)
{
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  cat('\t\t Name \t\t Top \t\t Bottom \t Thickness\n', sep='')
  
  for(k in 1:huf$nhuf)
  {
    cat('Layer ',k,':\t',huf$hgunam[k],'\t', huf$top[i, j, k], '\t', huf$top[i, j, k]-huf$thck[i, j, k],'\t', huf$thck[i, j, k],'\n', sep='')
  }
}
