#' Plot a MODFLOW 2D array
#' 
#' \code{plot.mf2darray} plots a MODFLOW 2D array.
#' 
#' @param mf2darray An object of class mf2darray, or a 2D matrix
#' @param ibound An ibound array with 1 or TRUE indicating active cells, and 0 or F indicating inactive cells
#' @param color.palette A color palette for imaging the parameter values
#' @param zlim
#' @param levels
#' @param nlevels
#' @param main
#' @return None
#' @method plot mf2darray
#' @export
#' @import ggplot2 directlabels akima rgl RTOOLZ
plot.mf2darray <- function(mf2darray, dis, ibound=mf2darray*0+1, color.palette=rev_rainbow, zlim = range(mf2darray, finite=TRUE), levels = pretty(zlim, nlevels), nlevels = 7, main='MF ARRAY plot', type='fill', add=FALSE,xOrigin=0,yOrigin=0,plot3d=FALSE,height.exageration=100,binwidth=1,label=TRUE)
{
  if(plot3d)
  {
    x <- (cumsum(dis$DELR)-dis$DELR/2)
    y <- sum(dis$DELC) - (cumsum(dis$DELC)-dis$DELC/2)
    z <- t(mf2darray)*height.exageration
    if(!add) open3d()
    colorlut <- color.palette(nlevels) # height color lookup table
    
    col <- colorlut[ round(approx(seq(zlim[1],zlim[2],length=nlevels+1),seq(0.5,nlevels+0.5,length=nlevels+1),xout=c(z/height.exageration),rule=2)$y) ] # assign colors to heights for each point
    surface3d(x,y,z,color=col,back='lines') 
  } else
  {
    xy <- expand.grid(cumsum(dis$DELR)-dis$DELR/2,sum(dis$DELC)-(cumsum(dis$DELC)-dis$DELC/2))
    names(xy) <- c('x','y')
    xy$x <- xy$x + xOrigin
    xy$y <- xy$y + yOrigin
    ibound[which(ibound==0)] <- NA
    
    if(type=='fill')
    {  
      ids <- factor(1:(dis$NROW*dis$NCOL))
      xWidth <- rep(dis$DELR,dis$NROW)
      yWidth <- rep(dis$DELC,each=dis$NCOL)
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(xy$y,each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c(t(mf2darray*ibound^2)))
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
      if(add)
      {
        return(geom_polygon(aes(x=x,y=y,fill=value, group=id),data=datapoly) +
          scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim))
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
          geom_polygon(aes(fill=value, group=id)) +
          scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim))
      }
    }
    if(type=='contour')
    {
      xy$z <- c(t(mf2darray*ibound^2))
      xyBackup <- xy
      xy <- na.omit(xy)
      xy <- interp(xy$x,xy$y,xy$z,xo=seq(min(xy$x),max(xy$x),length=ceiling(sum(dis$DELR)/min(dis$DELR))),yo=seq(min(xy$y),sum(max(xy$y)),length=ceiling(sum(dis$DELC)/min(dis$DELC))))
      xy$x <- rep(xy$x,ceiling(sum(dis$DELC)/min(dis$DELC)))
      xy$y <- rep(xy$y,each=ceiling(sum(dis$DELR)/min(dis$DELR)))
      xy$z <- c(xy$z)
      xy <- as.data.frame(xy)
      xy <- xy[which(xy$z >= zlim[1] & xy$z <= zlim[2]),]
      closestGridPoints <- apply(xy[,c('x','y')],1,function(x) which.min((x[1]-xyBackup$x)^2 + (x[2]-xyBackup$y)^2))
      xy$z[which(is.na(xyBackup$z[closestGridPoints]))] <- NA
      rm(xyBackup)
      if(add)
      {
        if(label) return(stat_contour(aes(x=x,y=y,z=z,colour = ..level..),data=xy,binwidth=binwidth))
        if(!label) return(stat_contour(aes(x=x,y=y,z=z),colour='black',data=xy,binwidth=binwidth))
      } else {
        if(label) return(direct.label(ggplot(xy, aes(x=x, y=y, z=z)) + stat_contour(aes(colour = ..level..),binwidth=binwidth)))
        if(!label) return(ggplot(xy, aes(x=x, y=y, z=z)) + stat_contour(colour = 'black',binwidth=binwidth))
      }
    }
  }
}