#' Add rmf array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @param dim the dim attribute for the array to be created; by default, dim(obj) is used
#' @param kper integer vector specifying the stress periods in which the array is active. Used for defining boundary conditions. Defaults to \code{NULL}
#' @param ... 
#' @details subsetting a \code{rmf_array} will return a \code{rmf_array} as long as the object has a dim argument. Atomic vectors are therefore never \code{rmf_arrays}. 
#'          By default, \code{drop = TRUE} for the subsetting, except for \code{rmf_4d_array}. This is done so that a \code{rmf_4d_array} where the 3th dimension has length 1 
#'          will not loose this dimension which would result in a \code{rmf_3d_array} where the 3th dimension represents time instead of MODFLOW layers.
#' @export
rmf_create_array <- function(obj = NA, dim = NULL, kper = NULL) {
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
  attr(obj, 'kper') <- kper
  return(obj)
}

#' @describeIn rmf_create_array Deprecated function name
#' @export
create_rmodflow_array <- function(...) {
  .Deprecated(new = "rmf_create_array", old = "create_rmodflow_array")
  rmf_create_array(...)
}

#' @export
"[.rmf_4d_array" <-  function(x, i, j, k, l, ...) {
  if(missing(i) && missing(j) && missing(k) && missing(l)) return(x)
  # 4d not collapsed to 3d if 3th dimension has length 1 
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], ifelse(missing(k) && dim(x)[3] == 1, FALSE, TRUE))
  obj <-  NextMethod(..., drop = drop)
  if (length(dim(obj)) == 2) {
    class(obj) <- replace(class(x), class(x) == 'rmf_4d_array', 'rmf_2d_array')
  }
  else if (length(dim(obj)) == 3) {
    class(obj) <- replace(class(x), class(x) == 'rmf_4d_array', 'rmf_3d_array')
  } 
  else if (length(dim(obj)) == 4) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), class(x) != 'rmf_4d_array')
  }
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  return(obj)
}

#' @export
"[.rmf_3d_array" <-  function(x, i, j, k, ...) {
  if(missing(i) && missing(j) && missing(k)) return(x)
  
  obj <-  NextMethod(...)
  if (length(dim(obj)) == 2) {
    class(obj) <- replace(class(x), class(x) == 'rmf_3d_array', 'rmf_2d_array')
  }
  else if (length(dim(obj)) == 3) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), class(x) != 'rmf_3d_array')
  }
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  return(obj)
}

#' @export
"[.rmf_2d_array" <-  function(x, i, j, ...) {
  if(missing(i) && missing(j)) return(x)
  
  obj <-  NextMethod(...)

   if (length(dim(obj)) == 2) {
     class(obj) <- class(x)
   } else {
     class(obj) <- subset(class(x), class(x) != 'rmf_2d_array')
   }
  
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])

  return(obj)
}

