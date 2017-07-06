#' Add rmf array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @param dim the dim attribute for the array to be created; by default, dim(obj) is used
#' @export
rmf_create_array <- function(obj = NA, dim = NULL) {
  if(!is.null(dim)) obj <- array(obj, dim = dim)
  if(length(dim(obj))==2) {
    class(obj) <- 'rmf_2d_array'
  } else if(length(dim(obj))==3) {
    class(obj) <- 'rmf_3d_array'
  } else if(length(dim(obj))==4) {
    class(obj) <- 'rmf_4d_array'
  } else {
    stop('Please provide 2d matrix, or 2d, 3d or 4d array.')
  }
  return(obj)
}

#' @describeIn rmf_create_array Deprecated function name
create_rmodflow_array <- function(...) {
  .Deprecated(new = "rmf_create_array", old = "create_rmodflow_array")
  rmf_create_array(...)
}
