#' Convert modflow coordinates to real world coordinates
#' 
#' @param x modflow x coordinate
#' @param y modflow y coordinate
#' @param z modflow z coordinate
#' @param i modflow row number
#' @param j modflow column number
#' @param k modflow layer number
#' @param roff modflow row offset
#' @param coff modflow column offset
#' @param loff modflow layer offset
#' @param prj prj object
#' @param dis dis object
#' @details Provide either xyz or ijk
#' @return data frame with real world x and y coordinates
#' @export
rmf_convert_grid_to_xyz <- function(x = NULL,
                                    y = NULL,
                                    z = NULL,
                                    i = NULL,
                                    j = NULL,
                                    k = NULL,
                                    roff = NULL,
                                    coff = NULL,
                                    loff = NULL,
                                    prj,
                                    dis = NULL) {
  if(!is.null(x)) {
    if(length(prj$origin) <= 2) prj$origin <-  c(prj$origin, 0)
    
    # counterclockwise rotation is negative. Now corresponds to rmf_convert_xyz_to_grid
    angle <- atan(y/x)*180/pi+prj$rotation
    angle[which(is.na(angle))] <- 90-prj$rotation
    s <- sqrt(x^2+y^2)
    x <- prj$origin[1]+ cos(angle*pi/180)*s
    y <- prj$origin[2]+ sin(angle*pi/180)*s
    if(!is.null(z)) z <- prj$origin[3]+z
    ifelse(!is.null(z),return(data.frame(x=x,y=y,z=z)),return(data.frame(x=x,y=y)))
  } else if(!is.null(i)) {
    #  vectorize
    y_grid <- c(cumsum(rev(dis$delc))-rev(dis$delc)/2)[(dis$nrow-i+1)]
    x_grid <- c(cumsum(dis$delr)-dis$delr/2)[j]
    if(!is.null(k)) { # need to adjust nlay for semi-confining beds
      thck <-  dis$botm
      thck[,,1] <- dis$top - dis$botm[,,1]
      thck[,,2:dis$nlay] <- dis$botm[,,(2:dis$nlay-1)] - dis$botm[,,2:dis$nlay]
      z_grid <- sum(thck[i,j,k:dis$nlay])-thck[i,j,k]/2
      if(!is.null(loff)) z_grid <- z_grid - loff*thck[i,j,k]/2
    }
    
    # offset
    if(!is.null(roff)) y_grid <- y_grid - roff*dis$delc[i]
    if(!is.null(coff)) x_grid <- x_grid + coff*dis$delr[j]
    
    x <- x_grid
    y <- y_grid
    if(!is.null(k)) z <- z_grid
    
    if(!is.null(prj)) {
      if(length(prj$origin) <= 2) prj$origin = c(prj$origin, 0)
      s <- sqrt(x_grid^2+y_grid^2)
      angle <- asin(y_grid/s)*180/pi - prj$rotation
      x_grid <- cos(angle*pi/180)*s
      y_grid <- sin(angle*pi/180)*s
      x <- prj$origin[1] + x_grid
      y <- prj$origin[2] + y_grid
      if(!is.null(k)) {
        z <- prj$origin[3] + z_grid
      }
    }
    
    dat <- data.frame(x=x,y=y)
    if(!is.null(k)) dat$z = z
    return(dat)
  }
}                                                                           

#' @describeIn rmf_convert_grid_to_xyz Deprecated function name
#' @export
convert_grid_to_xyz <- function(...) {
  .Deprecated(new = "rmf_convert_grid_to_xyz", old = "convert_grid_to_xyz")
  rmf_convert_grid_to_xyz(...)
}
