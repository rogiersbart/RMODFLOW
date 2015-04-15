#' Convert ijk to id
#' 
#' @export
convert_ijk_to_id <- function(i,j,k,dis)
{
  return((k-1)*dis$NROW*dis$NCOL+(i-1)*dis$NCOL+j)
}