#' Get information from a huf object at a certain grid node
#' 
#' @return \code{NULL}
#'
#' @rdname node_info
#' @method node_info huf
#' @export
node_info.huf <- function(huf, i, j)
{
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  cat('\t\t Name \t\t Top \t\t Bottom \t Thickness\n', sep='')
  
  for(i in 1:huf$NHUF)
  {
    cat('Layer ',i,':\t',huf$HGUNAM[i],'\t', huf$TOP[i, j, i], '\t', huf$TOP[i, j, i]-huf$THCK[i, j, i],'\t', huf$THCK[i, j, i],'\n', sep='')
  }
}