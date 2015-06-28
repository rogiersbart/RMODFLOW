#' Convert ijk to id
#' 
#' @param i vector of row numbers
#' @param j vector of column numbers
#' @param k vector of layer numbers
#' @param dis a discretization file object
#' @return cell ids, providing the place of the cell in an input file 3d array (not like the way R uses ids for arrays or matrices; rows and columns are switched)
#' @export
convert_ijk_to_id <- function(i,j,k,dis)
{
  return((k-1)*dis$NROW*dis$NCOL+(i-1)*dis$NCOL+j)
}