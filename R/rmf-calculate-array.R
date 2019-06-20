
#' Calculate a \code{rmf_2d_array} from multiplier arrays, zone arrays and/or parameter values
#'
#' Given a multiplier array and/or zone array with corresponding zone numbers, calculate a \code{rmf_2d_array}. Parameter values can be used to multiply the arrays as well.
#' 
#' @param dis dis object; used to set the dimensions of the array
#' @param mltarr either a list of multiplier arrays or a single multiplier array. The keyword \code{"NONE"} indicates no multiplier array is present. 
#' @param zonarr either a list of the zone arrays or a single zone array. The keyword \code{"ALL"} indicates no zone array is present.
#' @param iz only read when zonarr is not \code{"ALL"}. A list where each element is a numeric vector with the zone numbers for the corresponding zone array.
#' @param parval vector of parameter values corresponding to mltarr and/or zonarr which are multiplied with the final array. Typically used when the array represents a parameter.
#' @details if mltarr (zonarr) is a list, certain elements are allowed to be set to \code{"NONE"} (\code{"ALL"}) indicating the multiplier (zone) array is not active for this cluster.
#'          if mltarr is \code{"NULL"} and zonarr is \code{"ALL"}, all the cells in the returned array will be equal to the parameter value.
#'          Multiple multiplier arrays, zone arrays and/or parameter values can be used to calculate the final values. Cells that correspond to multiple multiplier arrays, zone arrays and/or parameter values will be summed.
#' @return \code{rmf_2d_array} with the values calculated from the multiplier and/or zone arrays.
#' @export

rmf_calculate_array <-  function(dis,
                               mltarr,
                               zonarr, 
                               iz = NULL,
                               parval = 1.0) {
  
  if(is.array(mltarr)) mltarr <- list(mltarr)
  if(is.array(zonarr)) zonarr <- list(zonarr)
  nclu <- max(length(mltarr), length(zonarr), length(parval))
  if(nclu > 1) {
    if(length(parval) == 1) parval <- rep(parval, nclu)
    if(length(mltarr) == 1) mltarr <- rep(mltarr, nclu)
    if(length(zonarr) == 1) zonarr <- rep(zonarr, nclu)
  }
  
  dim <- c(dis$nrow, dis$ncol)
  
  # function to create the array
  set_parm <- function(dim, mult, zone, iz, p_value) {

    if(!is.array(mult)) mult <- array(1.0, dim = dim)
    
    zon_l <- array(TRUE, dim = dim)
    if(is.array(zone)) zon_l[!(zone %in% iz)] <- FALSE
    
    mult[zon_l] <- mult[zon_l]*p_value
    mult[!zon_l] <- NA
    return(mult)
  }
  
  # create the array for every cluster then sum the clusters
  arr <- lapply(1:nclu, function(i) set_parm(dim = dim, mult = mltarr[[i]], zone = zonarr[[i]], iz = iz[[i]], p_value = parval[i]))
  arr <- rmf_create_array(apply(abind::abind(arr, along = 3), c(1,2), sum, na.rm = TRUE))
  attr(arr, 'dimnames') <- NULL
  return(arr)
}
