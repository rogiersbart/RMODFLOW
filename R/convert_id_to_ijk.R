#' Convert id to ijk
#' 
#' @param id cell id, providing the place of the number in an input file 3d array (not like the way R uses ids for arrays or matrices; rows and columns are switched)
#' @param dis a discretisation file object
#' 
#' @export
convert_id_to_ijk <- function(id,dis)
{
  k <- id %/% (dis$NROW*dis$NCOL)
  id <- id-k*(dis$NROW*dis$NCOL)
  i <- id %/% dis$NCOL
  j <- id-i*dis$NCOL
  return(data.frame(i=i+1,j=j,k=k+1))
}