
#'
#' Concise function for creating a 3D-array parameter.
#' 
#' Create a list of MODFLOW parameters from a 3D-array.
#' 
#' @param array a \code{rmf_3d_array} object
#' @param parnam character specifying the name of the parameter
#' @param parval numeric specifying the value of the parameter which is used to multiply values in the array. Defaults to 1.0
#' @param layer integer vector denoting the layer indices represented by the parameter. 
#' @param type character specifying the type of flow parameter. Allowed values are \code{HK, HANI, VK, VANI, SS, SY and VKCB}. 
#' @param mltnam character vector specifying the name of the resulting multiplier array. 
#' @param instnam optional character specying the instance name of the parameter is to be time-varying; defaults to NULL
#' 
#' @details This function will only set the multiplier arrays for all layers of the 3D array. The user must make sure that this multiplier array is written to a separate MLT file when running a MODFLOW simulation.
#' This function is intended to be used when no multiplier and/or zone arrays are specified for a parameter.
#' @return a list of \code{rmf_2d_array} objects of class \code{rmf_parameter}, one for each layer.
#' @export
#' 

rmf_create_parameter.rmf_3d_array <-  function(array,
                                               parnam, 
                                               parval = 1.0,
                                               layer = 1:dim(array)[3],
                                               type,
                                               mltnam = parnam,
                                               instnam = NULL) {
  # subset if not all layers are needed
  if(dim(array)[3] != length(layer)) array <- array[,,layer]
  unq <- vapply(1:dim(array)[3], function(i) length(unique(c(array[,,i]))) == 1, TRUE)
  if(length(mltnam) == 1) mltnam <- paste(mltnam, layer, sep = '_')
  mltnam[unq] <- 'NONE'
   
  attr(array, 'parnam') <- parnam
  attr(array, 'parval') <- parval
  attr(array, 'layer') <- layer
  attr(array, 'type') <- type
  attr(array, 'mlt') <- mltnam
  attr(array, 'zon') <- rep('ALL', length(layer))
  attr(array, 'iz') <- NULL
  attr(array, 'instnam') <- instnam
  class(array) <- c('rmf_parameter', class(array))
  
  return(array)
  
}