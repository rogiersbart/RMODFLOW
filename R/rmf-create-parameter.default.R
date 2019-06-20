
#' Create an array parameter
#'
#' Create a parameter from multiplier and/or zone arrays used in MODFLOW boundary condition packages and flow packages
#'
#' @param dis \code{RMODFLOW} dis object. Used to dimension the final array.
#' @param kper integer vector with the stress period numbers during which the parameter is active. Specifying kper indicates that this parameter represent boundary condition information.
#' @param layer integer vector denoting the layer indices represented by the parameter. Specifying layer indicates that this parameter represent flow package information.
#' @param parnam character specifying the name of the parameter
#' @param parval numeric specifying the value of the parameter which is used to multiply values in the array. Defaults to 1.0
#' @param mltnam character vector specifying the names of multiplier arrays that are used to build the parameter. The keyword \code{"NONE"} indicates no multiplier array is present. 
#' @param zonnam character vector specifying the names of zone arrays that are used to build the parameter. The keyword \code{"ALL"} indicates no multiplier array is present. 
#' @param type character specifying the type of flow parameter. Allowed values are \code{HK, HANI, VK, VANI, SS, SY and VKCB}. Not used when \code{layer} is \code{NULL}.
#' @param mlt \code{RMODFLOW} mlt object which holds the multiplier arrays specified in \code{mltnam}
#' @param zon \code{RMODFLOW} zon object which holds the zone arrays specified in \code{zonnam}
#' @param iz only read when the corresponding \code{zonnam} is not \code{"ALL"}. A list where each element is a numeric vector with the zone numbers for the corresponding zone array.
#' @param instnam optional character specying the instance name of the parameter is to be time-varying; defaults to NULL
#' @details if the parameter is to be time-varying, a separate parameter should be created for each instance with a unique \code{instnam} but with the same \code{name} 
#'          Typically, an array parameter is build from a single multiplier and/or zone array combination. However, multiple combinations can be used.
#'          If \code{mltnam} is \code{"NONE"} and \code{zonnam} is \code{"ALL"}, \code{parval} applies to all cells in the array and \code{mlt} and \code{zon} are not read.
#' @return an \code{rmf_2d_array} object of class \code{rmf_parameter}
#' @export
#' @seealso \code{\link{rmf_create_array}}
#'
rmf_create_parameter.default <- function(dis,
                                         kper = NULL,
                                         layer = NULL,
                                         parnam,
                                         parval = 1.0, 
                                         mltnam = 'NONE', 
                                         zonnam = 'ALL', 
                                         type = NULL,
                                         mlt = NULL, 
                                         zon = NULL, 
                                         iz = NULL, 
                                         instnam = NULL) {
  
  if(is.null(kper) && is.null(layer)) stop("Please specify either the kper argument (for boundary condition arrays) or the layer argument (for flow parameter arrays).")
  if(!is.null(layer) && is.null(type)) stop("Please specify the parameter type")
  mltarr <- list(mltnam)
  zonarr <- list(zonnam)
  
  if(any(toupper(mltnam) != "NONE")) {
    if(is.null(mlt)) stop('Please provide a mlt object')
    mltarr[which(toupper(mltnam) != "NONE")] <- mlt$rmlt[which(mlt$mltnam %in% mltnam)]
  }
  if(any(toupper(zonnam) != "ALL")) {
    if(is.null(zon)) stop('Please provide a zon object')
    if(is.null(iz)) stop('Please provide a iz argument')
    zonarr[which(toupper(zonnam) != "ALL")] <- zon$izon[which(zon$zonnam %in% zonnam)]
  }
  
  
  arr <- rmf_calculate_array(dis = dis,
                             mltarr = mltarr,
                             zonarr = zonarr,
                             iz = iz,
                             parval = parval)
  
  attr(arr, 'kper') <- kper
  attr(arr, 'parnam') <- parnam
  attr(arr, 'parval') <- parval
  attr(arr, 'layer') <- layer
  attr(arr, 'type') <- type
  attr(arr, 'mlt') <- mltnam
  attr(arr, 'zon') <- zonnam
  attr(arr, 'iz') <- iz
  attr(arr, 'instnam') <- instnam
  class(arr) <- c('rmf_parameter', class(arr))
  return(arr)
  
}