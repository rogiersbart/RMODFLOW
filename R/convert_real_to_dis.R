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
convert_real_to_dis <- function(x,y,prj,z=NULL,dis=NULL)
{
  x <- x-prj$origin[1]
  y <- y-prj$origin[2]
  angle <- atan(y/x)*180/pi - prj$rotation
  angle[which(is.na(angle))] <- 90-prj$rotation
  s <- sqrt(x^2+y^2)
  x <- cos(angle*pi/180)*s
  y <- sin(angle*pi/180)*s
  if(!is.null(z)) z <- z - prj$origin[3]
  dat <- data.frame(x=x,y=y)
  if(!is.null(z)) dat$z <- z
  if(!is.null(dis))
  {
    if(ncol(dat)==3)
    {
      dis$THCK <- dis$TOPS <- dis$BOTM
      dis$THCK[,,1] <- dis$TOP - dis$BOTM[,,1]
      dis$TOPS[,,1] <- dis$TOP
      for(k in 2:dis$NLAY)
      {
        dis$THCK[,,k] <- dis$BOTM[,,(k-1)] - dis$BOTM[,,k]
        dis$TOPS[,,k] <- dis$BOTM[,,(k-1)]
      }
    }
    for(i in 1:nrow(dat))
    {
      dat$i[i] <- which(cumsum(dis$DELC) > sum(dis$DELC)-dat$y[i])[1]
      dat$j[i] <- which(cumsum(dis$DELR) > dat$x[i])[1]
      
      dat$roff[i] <- (sum(dis$DELC)-dat$y[i] -(cumsum(dis$DELC) - dis$DELC/2)[dat$i[i]])/dis$DELC[dat$i[i]]
      dat$coff[i] <- (dat$x[i] -(cumsum(dis$DELR) - dis$DELR/2)[dat$j[i]])/dis$DELR[dat$j[i]]
      if(!is.null(z)) 
      {
        dat$k[i] <- which(dis$BOTM[dat$i[i],dat$j[i],] < dat$z[i])[1]
        dat$loff[i] <- -(dat$z[i]-(dis$TOPS[dat$i[i],dat$j[i],dat$k[i]]+dis$BOTM[dat$i[i],dat$j[i],dat$k[i]])/2)/dis$THCK[dat$i[i],dat$j[i],dat$k[i]]
      }
    }
  }
  return(dat)
}
