#'
#' Convert objects to a rmf_list
#'
#' @rdname rmf_as_list
#' @export

rmf_as_list <- function(...) {
  UseMethod('rmf_as_list')
}

#'
#' Converts a rmf_2d_array or rmf_3d_array to a rmf_list
#'
#' @param obj a \code{rmf_2d_array} or \code{rmf_3d_array}
#' @param dis a \code{RMODFLOW dis} object; only used when ijk is suppplied
#' @param name character; name of the resulting column which holds the extracted values from the array; defaults to "value"
#' @param mask a logical array with the same dimensions as obj. Used to select cells from obj; defaults to all cells
#' @param ijk optional; a data.frame with i, j and k columns used to select the cells. Overwrites the use of mask.
#' @param kper sets the kper attribute of the returned list; defaults to the kper attribute of obj
#' 
#' @details \code{\link{rmf_convert_id_to_ijk}} can be used to obtain a ijk data.frame from either MODFLOW cell id's or R cell id's.
#' 
#' @return a \code{rmf_list}
#' @rdname rmf_as_list
#' @export
#' @seealso \code{\link{rmf_as_array}}


rmf_as_list.rmf_3d_array <- function(obj, 
                                     dis, 
                                     name = "value",
                                     mask = 1 + obj*0, 
                                     ijk = NULL,
                                     kper = attr(obj, 'kper')) {
  
  if(!is.null(ijk)) {
    id <- rmf_convert_ijk_to_id(i = ijk$i, j = ijk$j, k = ijk$k, dis = dis, type = 'r') 
    values <- obj[id, drop = TRUE]
    
    lst <- cbind(ijk$i, ijk$j, ijk$k, values)
    colnames(lst) <- c("i", "j", "k", name)
    
  } else {
    id <- which(as.logical(mask))
    values <- obj[id, drop = TRUE]
    
    lst <- cbind(arrayInd(id, .dim = dim(obj)), values)
    colnames(lst) <- c("i", "j", "k", name)
    
  }
  
  return(rmf_create_list(lst, kper = kper))
}

#'
#' @rdname rmf_as_list
#' @export
rmf_as_list.rmf_2d_array <- function(obj, ...) {
  obj <- rmf_create_array(obj, dim = c(dim(obj), 1))
  rmf_as_list(obj, ...)
}

#' 
#' @param l integer index used to subset the 4th dimension of a \code{rmf_4d_array}.
#'
#' @rdname rmf_as_list
#' @export
rmf_as_list.rmf_4d_array <- function(obj, l, ...) {
  rmf_as_list(obj[,,,l], ...)
}




