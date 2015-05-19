#' Plot a MODFLOW 2D array
#' 
#' \code{plot.modflow_2d_array} plots a MODFLOW 2D array.
#' 
#' @param modflow_2d_array An object of class modflow_2d_array, or a 2D matrix
#' @param mask An ibound array with 1 or TRUE indicating active cells, and 0 or F indicating inactive cells
#' @param color.palette A color palette for imaging the parameter values
#' @param zlim
#' @param levels
#' @param nlevels
#' @param main
#' @return None
#' @method plot modflow_2d_array
#' @export
#' @import ggplot2 directlabels akima rgl RTOOLZ
plot.modflow_2d_array <- function(modflow_2d_array, dis, ba6=NULL, mask=ifelse0(is.null(ba6),modflow_2d_array*0+1,ba6$IBOUND[,,1]), color.palette=rev_rainbow, zlim = range(modflow_2d_array, finite=TRUE), levels = pretty(zlim, nlevels), nlevels = 7, main='MF ARRAY plot', type='fill', add=FALSE,xOrigin=0,yOrigin=0,height.exageration=100,binwidth=round(diff(zlim)/20),label=TRUE,prj=NULL,target_CRS=NULL,alpha=1,plot3d=FALSE,height=NULL)
{
  if(plot3d)
  {
    x <- (cumsum(dis$DELR)-dis$DELR/2)
    y <- sum(dis$DELC) - (cumsum(dis$DELC)-dis$DELC/2)
    z <- t(height)*height.exageration
    if(!add) open3d()
    colorlut <- colorRampPalette(color.palette(nlevels))(25) # height color lookup table
    col <- colorlut[ round(approx(seq(zlim[1],zlim[2],length=25+1),seq(0.5,25+0.5,length=25+1),xout=c(t(modflow_2d_array)),rule=2)$y) ] # assign colors to heights for each point
    alpha <- rep(1,length(col))
    alpha[which(c(t(mask))==0)] <- 0
    if(type=='fill') surface3d(x,y,z,color=col,alpha=alpha,back='lines',smooth=FALSE) 
    if(type=='grid') surface3d(x,y,z,front='lines',alpha=alpha,back='lines',smooth=FALSE) 
  } else
  {
    xy <- expand.grid(cumsum(dis$DELR)-dis$DELR/2,sum(dis$DELC)-(cumsum(dis$DELC)-dis$DELC/2))
    names(xy) <- c('x','y')
    xy$x <- xy$x + xOrigin
    xy$y <- xy$y + yOrigin
    mask[which(mask==0)] <- NA
    
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
      values <- data.frame(id = ids,value = c(t(modflow_2d_array*mask^2)))
      if(!is.null(prj))
      {
        new_positions <- convert_dis_to_real(x=positions$x,y=positions$y,prj=prj)
        positions$x <- new_positions$x
        positions$y <- new_positions$y
      }
      if(!is.null(target_CRS))
      {
        new_positions <- addosmmerc(data.frame(x=positions$x,y=positions$y),CRS_from=CRS(prj$projection),CRS_to=target_CRS)
        positions$x <- new_positions$Xosmmerc
        positions$y <- new_positions$Yosmmerc                    
      }
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
      if(add)
      {
        return(geom_polygon(aes(x=x,y=y,fill=value, group=id),data=datapoly,alpha=alpha))# +
          #scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim)) # solve this issue!
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
          geom_polygon(aes(fill=value, group=id),alpha=alpha) +
          scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim) +
          coord_equal())
      }
    }
    if(type=='grid')
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
      values <- data.frame(id = ids,value = c(t(modflow_2d_array*mask^2)))
      if(!is.null(prj))
      {
        new_positions <- convert_dis_to_real(x=positions$x,y=positions$y,prj=prj)
        positions$x <- new_positions$x
        positions$y <- new_positions$y
      }
      if(!is.null(target_CRS))
      {
        new_positions <- addosmmerc(data.frame(x=positions$x,y=positions$y),CRS_from=CRS(prj$projection),CRS_to=target_CRS)
        positions$x <- new_positions$Xosmmerc
        positions$y <- new_positions$Yosmmerc                    
      }
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
      if(add)
      {
        return(geom_polygon(aes(x=x,y=y,group=id),data=datapoly,alpha=alpha,colour='black',fill=NA))# +
        #scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim)) # solve this issue!
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
                 geom_polygon(aes(group=id),alpha=alpha,colour='black',fill=NA) +
                 coord_equal())
      }
    }
    if(type=='contour')
    {
      xy$z <- c(t(modflow_2d_array*mask^2))
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
        if(label) return(stat_contour(aes(x=x,y=y,z=z,colour = ..level..),data=xy,binwidth=binwidth)) # issue: direct.label does not work on ggplot layers, or at least not after fill plot # +geom_dl(aes(label=..level..),method="top.pieces", stat="contour")
        if(!label) return(stat_contour(aes(x=x,y=y,z=z),colour='black',data=xy,binwidth=binwidth))
      } else {
        if(label) return(ggplot(xy, aes(x=x, y=y, z=z)) + stat_contour(aes(colour = ..level..),binwidth=binwidth) +geom_dl(aes(label=..level.., colour=..level..),method="top.pieces", stat="contour") + coord_equal() +
                           theme(legend.position="none"))
        if(!label) return(ggplot(xy, aes(x=x, y=y, z=z)) + stat_contour(colour = 'black',binwidth=binwidth) + coord_equal())
      }
    }
  }
}