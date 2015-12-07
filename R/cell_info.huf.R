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
  
  for(i in 1:huf$nhuf)
  {
    cat('Layer ',i,':\t',huf$hgunam[i],'\t', huf$top[i, j, i], '\t', huf$top[i, j, i]-huf$thck[i, j, i],'\t', huf$thck[i, j, i],'\n', sep='')
  }
}
