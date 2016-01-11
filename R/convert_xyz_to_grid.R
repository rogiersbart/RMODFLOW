#' Convert real world coordinates to modflow coordinates
#' 
#' @param x real world x coordinate
#' @param y real world y coordinate
#' @param prj prj object
#' @param z real world z coordinate; optional
#' @param dis dis object; optional
#' @details
#' If dis is not provided, only x, y and z coordinates are returned. If z is not provided, no third dimension coordinates are returned.
#' @return data frame with x, y, z, i, j, k, roff, coff and loff modflow coordinates
#' @export
convert_xyz_to_grid <- function(x,y,prj=NULL,z=NULL,dis=NULL) {
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
  if(!is.null(dis)) {
    if(ncol(dat)==3) {
      dis$thck <- dis$tops <- dis$botm
      dis$thck[,,1] <- dis$top - dis$botm[,,1]
      dis$tops[,,1] <- dis$top
      for(k in 2:dis$nlay) {
        dis$thck[,,k] <- dis$botm[,,(k-1)] - dis$botm[,,k]
        dis$tops[,,k] <- dis$botm[,,(k-1)]
      }
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
  }
  return(dat)
}
