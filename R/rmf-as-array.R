
#'
#' Convert an object to a RMODFLOW array
#'
#'
#' @rdname rmf_as_array
#' @export

rmf_as_array <- function(...) {
  UseMethod('rmf_as_array')
}

#'
#' Convert a rmf_list to a RMODFLOW array
#' 
#' @param obj a \code{rmf_list}
#' @param dis a \code{RMODFLOW dis} object
#' @param select either a single integer or character specifying which column to select from the \code{rmf_list}. Defaults to the 4th column, i.e. the first variable.
#' @param na_value value of the cells in the array which are not specified in obj; defaults to 0
#' @param sparse logical; indicating if the returned array should be 2D or 3D. See details. Defaults to TRUE.
#' @param kper sets the kper attribute of the returned array; defaults to the kper attribute of obj
#'
#' @details the dimension of the returned array is guessed from the supplied dis object. If there is only one unique k value in obj, 
#'  \code{sparse}, determines the dimensions of the returned array. If \code{sparse = TRUE}, a 2D array is returned. If \code{sparse = FALSE}, a 3D array is returned.
#'  When there is more than one unique k value in obj, a 3D array is always returned.
#'
#' @return either a \code{rmf_2d_array} or a \code{rmf_3d_array}
#' @rdname rmf_as_array
#' @export
#' @seealso \code{\link{rmf_as_list}}
#' 


rmf_as_array.rmf_list <- function(obj, 
                                  dis, 
                                  select = 4,
                                  na_value = 0,
                                  sparse = TRUE,
                                  kper = attr(obj, 'kper')) {
  
  
  if(length(unique(obj$k)) == 1 && sparse) {
    id <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = 1, dis = dis, type = 'r')
    ar <- rmf_create_array(na_value, dim = c(dis$nrow, dis$ncol), kper = kper)
  } else {
    id <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis, type = 'r')
    ar <- rmf_create_array(na_value, dim = c(dis$nrow, dis$ncol, dis$nlay), kper = kper)
  }
  
  ar[id] <- obj[[select]]
  return(ar)
  
}
