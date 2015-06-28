#' Add modflow_2d_array class to object
#' 
#' @param obj object to add class to
#' @export
as.modflow_2d_array <- function(obj)
{
  class(obj) <- 'modflow_2d_array'
  return(obj)
}