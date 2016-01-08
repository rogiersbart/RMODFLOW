#' Add rmodflow array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @param dim the dim attribute for the array to be created; by default, dim(obj) is used
#' @export
create_rmodflow_array <- function(obj = NA, dim = NULL) {
  if(!is.null(dim)) obj <- array(obj, dim = dim)
  if(length(dim(obj))==2) {
    class(obj) <- 'rmodflow_2d_array'
  } else if(length(dim(obj))==3) {
    class(obj) <- 'rmodflow_3d_array'
  } else if(length(dim(obj))==4) {
    class(obj) <- 'rmodflow_4d_array'
  } else {
    stop('Please provide 2d matrix, or 2d, 3d or 4d array.')
  }
  return(obj)
}
