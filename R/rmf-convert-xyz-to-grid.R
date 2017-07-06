#' Convert real world coordinates to modflow coordinates
#' 
#' @param x real world x coordinate
#' @param y real world y coordinate
#' @param z real world z coordinate; optional
#' @param prj prj object
#' @param dis dis object; optional
#' @param output character; containing 'xyz','ijk' and/or 'off' for the return of x, y, z, i, j, k, roff, coff and loff modflow coordinates
#' @details
#' If dis is not provided, only x, y and z coordinates are returned. If z is not provided, no third dimension coordinates are returned. For the x, y and z modflow coordinates, the origin is placed at the lower left corner of the grid.
#' @return data frame with modflow coordinates
#' @export
rmf_convert_xyz_to_grid <- function(x,y,prj=NULL,z=NULL,dis=NULL,output='xyz') {
  output_xyz <- grepl('xyz',output)
  output_ijk <- grepl('ijk',output)
  output_off <- grepl('off',output)
  if(!is.null(prj)) {
    x <- x-prj$origin[1]
    y <- y-prj$origin[2]
    angle <- atan(y/x)*180/pi - prj$rotation
    angle[which(is.na(angle))] <- 90-prj$rotation
    s <- sqrt(x^2+y^2)
    x <- cos(angle*pi/180)*s
    y <- sin(angle*pi/180)*s
    if(!is.null(z)) z <- z - prj$origin[3]
  }
  dat <- data.frame(x=x,y=y)
  if(!is.null(z)) dat$z <- z
  if(output_ijk | output_off) {
    if(is.null(dis)) error('Please provide dis argument ...')    
    if(ncol(dat)==3) {
      dis$thck <- dis$tops <- dis$botm
      dis$thck[,,1] <- dis$top - dis$botm[,,1]
      dis$tops[,,1] <- dis$top
      dis$thck[,,2:dis$nlay] <- dis$botm[,,(2:dis$nlay-1)] - dis$botm[,,2:dis$nlay]
      dis$tops[,,2:dis$nlay] <- dis$botm[,,(2:dis$nlay-1)]
    }
    for(i in 1:nrow(dat)) {
      dat$i[i] <- which(cumsum(dis$delc) > sum(dis$delc)-dat$y[i])[1]
      dat$j[i] <- which(cumsum(dis$delr) > dat$x[i])[1]
      dat$roff[i] <- (sum(dis$delc)-dat$y[i] -(cumsum(dis$delc) - dis$delc/2)[dat$i[i]])/dis$delc[dat$i[i]]
      dat$coff[i] <- (dat$x[i] -(cumsum(dis$delr) - dis$delr/2)[dat$j[i]])/dis$delr[dat$j[i]]
      if(!is.null(z)) {
        dat$k[i] <- which(dis$botm[dat$i[i],dat$j[i],] < dat$z[i])[1]
        dat$loff[i] <- -(dat$z[i]-(dis$tops[dat$i[i],dat$j[i],dat$k[i]]+dis$botm[dat$i[i],dat$j[i],dat$k[i]])/2)/dis$thck[dat$i[i],dat$j[i],dat$k[i]]
      }
    }
    if(output_xyz & output_ijk & output_off) return(dat)
    if(output_xyz & output_ijk & !output_off) return(dat[,c('x','y','z','i','j','k')])
    if(output_xyz & !output_ijk & output_off) return(dat[,c('x','y','z','roff','coff','loff')])
    if(!output_xyz & output_ijk & output_off) return(dat[,c('i','j','k','roff','coff','loff')])
    if(!output_xyz & !output_ijk & output_off) return(dat[,c('roff','coff','loff')])
    if(!output_xyz & output_ijk & !output_off) return(dat[,c('i','j','k')])
  } else {
    return(dat)
  }
}

#' @describeIn rmf_convert_xyz_to_grid Deprecated function name
#' @export
convert_xyz_to_grid <- function(...) {
  .Deprecated(new = "rmf_convert_xyz_to_grid", old = "convert_xyz_to_grid")
  rmf_convert_xyz_to_grid(...)
}
