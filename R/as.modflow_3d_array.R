#' Add modflow_3d_array class to object
#' 
#' @param obj object to add class to
#' @export
as.modflow_3d_array <- function(obj)
{
  class(obj) <- 'modflow_3d_array'
  return(obj)
}