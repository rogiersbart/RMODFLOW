#' Convert data frame coordinates to another coordinate reference system
#' 
#' @param dat data frame with x and y coordinates
#' @param from coordinate reference system of the data
#' @param to target coordinate reference system
#' @param names_from names from the data frame coordinate columns
#' @param names_to names to use for the converted coordinates
#' @return data frame with converted coordinates
#' @importFrom sf st_transform st_sfc st_multipoint st_crs st_coordinates
#' @keywords internal
rmfi_convert_coordinates <- function(dat, from, to, names_from=c('x','y'), names_to=names_from) {
  nrs <- which(!is.na(dat[[names_from[1]]]+dat[[names_from[2]]]))
 # dat_names <- names(dat)
  if(is.na(sf::st_crs(from)) || is.na(sf::st_crs(to))) stop('crs can not be NA when transforming')
  converted_coords <- sf::st_transform(sf::st_sfc(sf::st_multipoint(cbind(dat[,names_from[1]], dat[,names_from[2]])[nrs, ]), crs = sf::st_crs(from)), crs = sf::st_crs(to))
  converted_coords <- data.frame(sf::st_coordinates(converted_coords))[,c('X','Y')]
  
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
