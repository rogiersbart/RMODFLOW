#' Convert id to ijk
#' 
#' @param id cell id, providing the place of the number in an input file 3d array
#' @param dis a discretisation file object
#' @export
convert_id_to_ijk <- function(id,dis)
{
  k <- id %/% (dis$nrow*dis$ncol)
  id <- id-k*(dis$nrow*dis$ncol)
  j <- id %/% dis$nrow
  i <- id-j*dis$nrow
  return(data.frame(i=i,j=j+1,k=k+1))
}
