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
#' If the xyz coordinate falls on a boundary of two cells, the minimum ijk indices are returned. 
#'
#' If the z coordinate falls within a Quasi-3D confining bed, the layer index of the overlying model layer is returned. The loff value then represents the fractional distance from the center of the overlying model layer.
#' @return data frame with modflow coordinates
#' @export
rmf_convert_xyz_to_grid <- function(x,y,prj=NULL,z=NULL,dis=NULL,output='xyz') {
  output_xyz <- 'xyz' %in% output
  output_ijk <- 'ijk' %in% output
  output_off <- 'off' %in% output
  if(!is.null(prj)) {
    if(length(prj$origin) <= 2) prj$origin <-  c(prj$origin, 0)
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
  if(output_ijk || output_off) {
    if(is.null(dis)) stop('Please provide dis argument ...')    
    if(ncol(dat)==3) {
      dis$thck <- dis$tops <- dis$botm
      dis$thck[,,1] <- dis$top - dis$botm[,,1]
      dis$tops[,,1] <- dis$top
      nnlay <- dim(dis$botm)[3] 
      if(nnlay > 1) dis$thck[,,2:nnlay] <- dis$botm[,,(2:nnlay)-1] - dis$botm[,,2:nnlay]
      if(nnlay > 1) dis$tops[,,2:nnlay] <- dis$botm[,,(2:nnlay)-1]
    }
    if(any(dis$laycbd != 0)) {
      cbd <- rep(0, nnlay)
      cbd[cumsum(dis$laycbd+1)[dis$laycbd != 0]] <- 1
    }
    for(i in 1:nrow(dat)) {
      dat$i[i] <- which(cumsum(dis$delc) >= sum(dis$delc)-dat$y[i])[1]
      dat$j[i] <- which(cumsum(dis$delr) >= dat$x[i])[1]
      dat$roff[i] <- (sum(dis$delc)-dat$y[i] -(cumsum(dis$delc) - dis$delc/2)[dat$i[i]])/dis$delc[dat$i[i]]
      dat$coff[i] <- (dat$x[i] -(cumsum(dis$delr) - dis$delr/2)[dat$j[i]])/dis$delr[dat$j[i]]
      
      if(dat$x[i] < 0 || dat$x[i] > sum(dis$delr)) {
        dat$j[i] <- NA
        dat$roff[i] <- NA
        dat$coff[i] <- NA
        warning('x coordinate out of bounds. Setting j index and roff/coff to NA')
      } 
      if(dat$y[i] < 0 || dat$y[i] > sum(dis$delc)) {
        dat$i[i] <- NA
        dat$roff[i] <- NA
        dat$coff[i] <- NA
        warning('y coordinate out of bounds. Setting i index and roff/coff to NA')
      }

      if(!is.null(z)) {
        if(is.na(dat$i[i]) || is.na(dat$j[i])) {
          warning('i and/or j index is NA. Setting corresponding k index and loff to NA as well.')
          dat$k[i] <- NA
          dat$loff[i] <- NA
        } else if(dat$z[i] < dis$botm[dat$i[i], dat$j[i],nnlay] || dat$z[i] > dis$top[dat$i[i], dat$j[i]]) {
          warning('z coordinate out of bounds. Setting k index and loff to NA.')
          dat$k[i] <- NA
          dat$loff[i] <- NA
        } else {
          dat$k[i] <- which(dis$botm[dat$i[i],dat$j[i],] <= dat$z[i])[1]
          k_org <- dat$k[i]
          
          # adjust for cbd
          if(any(dis$laycbd != 0)) {
            dat$k[i] <- dat$k[i] - sum(cbd[1:dat$k[i]])
            if(dis$laycbd[dat$k[i]] != 0) k_org <- dat$k[i]
          }
          dat$loff[i] <- -(dat$z[i]-(dis$tops[dat$i[i],dat$j[i],k_org]+dis$botm[dat$i[i],dat$j[i],k_org])/2)/dis$thck[dat$i[i],dat$j[i],k_org]
        }
      }
    }
    
    columns = vector(mode = "character")
    if(output_xyz) {
      if(!is.null(z)){
        columns = append(columns, c('x','y','z'))
      } else {
        columns = append(columns, c('x','y'))
      }
    }
    if(output_ijk) {
      if(!is.null(z)) {
        columns = append(columns, c('i','j','k'))
      } else {
        columns = append(columns, c('i','j'))
      }
    }
    if(output_off) {
      if(!is.null(z)) {
        columns = append(columns, c('roff','coff','loff'))
      } else {
        columns = append(columns, c('roff','coff'))
      }
    }
    
    return(dat[,columns])
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
