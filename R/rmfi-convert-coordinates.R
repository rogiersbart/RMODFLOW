#' Convert data frame coordinates to another coordinate reference system
#' 
#' @param dat data frame with x and y coordinates
#' @param from coordinate reference system of the data
#' @param to target coordinate reference system
#' @param names_from names from the data frame coordinate columns
#' @param names_to names to use for the converted coordinates
#' @return data frame with converted coordinates
#' @importFrom sp spTransform SpatialPoints
rmfi_convert_coordinates <- function(dat, from, to, names_from=c('x','y'), names_to=names_from) {
  nrs <- which(!is.na(dat$x+dat$y))
  dat_names <- names(dat)
  if (length(nrs) > 1) {
    converted_coords <- sp::spTransform(sp::SpatialPoints((cbind(dat[,names_from[1]], dat[,names_from[2]])[nrs, ]), proj4string = from), to)
  } else {
    converted_coords <- sp::spTransform(sp::SpatialPoints(data.frame(cbind(dat[,names_from[1]], dat[,names_from[2]]))[nrs, ], proj4string = from), to)
  } 
  if(length(nrs) == 1) {
    converted_coords <- data.frame(converted_coords$X1,converted_coords$X2)
    
  } else {
    converted_coords <- data.frame(converted_coords$coords.x1,converted_coords$coords.x2)
  }
  names(converted_coords) <- names_to
  if(names_from[1]==names_to[1]) {
    dat[,names_to[1]][nrs] <- converted_coords[,1]
  } else {
    dat$new <- NA
    dat$new[nrs] <- converted_coords[,1]
    names(dat)[ncol(dat)] <- names_to[1]
  }
  if(names_from[2]==names_to[2]) {
    dat[,names_to[2]][nrs] <- converted_coords[,2]
  } else {
    dat$new <- NA
    dat$new[nrs] <- converted_coords[,2]
    names(dat)[ncol(dat)] <- names_to[2]
  }  
  return(dat)
}
