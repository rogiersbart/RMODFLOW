#' Convert id to ijk
#' 
#' @param id cell id, providing the place of the number in an input file 3d array
#' @param dis a discretisation file object
#' @param type 'r' or 'modflow'; defaults to 'r'
#' @details a modflow id provides the place of the number in an input file 3d array (not like the way R uses ids for arrays or matrices; rows and columns are switched)
#' @export
convert_id_to_ijk <- function(id,
                              dis,
                              type = 'r') {
  k <- id %/% (dis$nrow*dis$ncol)
  id <- id-k*(dis$nrow*dis$ncol)
  if(type == 'r') {
    j <- id %/% dis$nrow
    i <- id-j*dis$nrow
    return(data.frame(i=i,j=j+1,k=k+1))  
  } else if (type == 'modflow') {
    i <- id %/% dis$ncol
    j <- id-i*dis$ncol
    return(data.frame(i=i+1,j=j,k=k+1))
  } else {
    stop('Please provide a valid id type.')
  }  
}
