#' Add class to object
#' 
#' @export
as.modflow_3d_array <- function(obj)
{
  class(obj) <- 'modflow_3d_array'
  return(obj)
}