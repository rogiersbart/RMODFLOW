
#'
#' Concise function for creating a 2D-array parameter.
#' 
#' Create a MODFLOW parameter from a 2D-array.
#' 
#' @param array a \code{rmf_2d_array} object
#' @param parnam character specifying the name of the parameter
#' @param parval numeric specifying the value of the parameter which is used to multiply values in the array. Defaults to 1.0
#' @param kper integer vector with the stress period numbers during which the parameter is active. Specifying kper indicates that this parameter represent boundary condition information.
#' @param layer integer vector denoting the layer indices represented by the parameter. Specifying layer indicates that this parameter represent flow package information.
#' @param type character specifying the type of flow parameter. Allowed values are \code{HK, HANI, VK, VANI, SS, SY and VKCB}. Not used when \code{layer} is \code{NULL}.
#' @param mltnam character vector specifying the name of the resulting multiplier array. 
#' @param instnam optional character specying the instance name of the parameter is to be time-varying; defaults to NULL
#' 
#' @details This function will only set the multiplier array. The user must make sure that this multiplier array is written to a separate MLT file when running a MODFLOW simulation.
#' This function is intended to be used when no multiplier and/or zone arrays are specified for a parameter.
#' @return a \code{rmf_2d_array} object of class \code{rmf_parameter}
#' @export
#' 
rmf_create_parameter.rmf_2d_array <-  function(array,
                                               parnam, 
                                               parval = 1.0,
                                               kper = attr(array, 'kper'),
                                               layer = NULL,
                                               type = NULL,
                                               mltnam = parnam,
                                               instnam = NULL) {
  
  if(is.null(kper) && is.null(layer)) stop("Please specify either the kper argument (for boundary condition arrays) or the layer argument (for flow parameter arrays).")
  if(!is.null(layer) && is.null(type)) stop("Please specify the parameter type")
  
  if(length(unique(c(array))) == 1) mltnam <-  'NONE'
  
  attr(array, 'kper') <- kper
  attr(array, 'parnam') <- parnam
  attr(array, 'parval') <- parval
  attr(array, 'layer') <- layer
  attr(array, 'type') <- type
  attr(array, 'mlt') <- mltnam
  attr(array, 'zon') <- 'ALL'
  attr(array, 'iz') <- NULL
  attr(array, 'instnam') <- instnam
  class(array) <- c('rmf_parameter', class(array))
  return(array)
}