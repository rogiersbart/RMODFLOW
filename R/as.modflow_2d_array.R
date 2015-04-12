#' Add class to object
#' 
#' @export
as.modflow_2d_array <- function(obj)
{
  class(obj) <- 'modflow_2d_array'
  return(obj)
}