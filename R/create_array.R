#' Add rmodflow array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @export
create_array <- function(obj) {
  if(length(dim(obj))==2) {
    class(obj) <- '2d_array'
  } else if(length(dim(obj))==3) {
    class(obj) <- '3d_array'
  } else if(length(dim(obj))==4) {
    class(obj) <- '4d_array'
  } else {
    stop('Please provide 2d matrix, or 2d, 3d or 4d array.')
  }
  return(obj)
}
