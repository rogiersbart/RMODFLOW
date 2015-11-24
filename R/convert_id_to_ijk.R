#' Convert id to ijk
#' 
#' @param id cell id, providing the place of the number in an input file 3d array
#' @param dis a discretisation file object
#' @export
convert_id_to_ijk <- function(id,dis)
{
  k <- id %/% (dis$NROW*dis$NCOL)
  id <- id-k*(dis$NROW*dis$NCOL)
  j <- id %/% dis$NROW
  i <- id-j*dis$NROW
  return(data.frame(i=i,j=j+1,k=k+1))
}
