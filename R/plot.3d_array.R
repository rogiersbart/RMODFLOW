#' Plot a 2D section through a MODFLOW 3D array
#' 
#' \code{plot.3d_array} plots a 2D section through a MODFLOW 3D array.
#' 
#' @param rmodflow_array an object of class 3d_array
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param dis discretization file object
#' @param bas basic file object; optional
#' @param mask a 3D array with 0 or F indicating inactive cells optional; defaults to having all cells active or, if bas is provided, bas$ibound
#' @param colour_palette a colour palette for imaging the array values
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param ... parameters provided to plot.2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method plot 3d_array
#' @export
plot.3d_array <- function(rmodflow_array, i=NULL, j=NULL, k=NULL, dis, bas=NULL, mask=ifelse0(is.null(bas),rmodflow_array*0+1,bas$ibound), zlim = range(rmodflow_array[ifelse0(is.null(i),c(1:dim(rmodflow_array)[1]),i),ifelse0(is.null(j),c(1:dim(rmodflow_array)[2]),j),ifelse0(is.null(k),c(1:dim(rmodflow_array)[3]),k)], finite=TRUE), colour_palette=rev_rainbow, nlevels = 7, type='fill', add=FALSE, ...)
{
  if(!is.null(k))
  {
    rmodflow_array <- rmodflow_array[,,k]
    class(rmodflow_array) <- '2d_array'
    mask <- mask[,,k]
    plot(rmodflow_array, dis, mask=mask, zlim=zlim, type=type, add=add, ...)
  } else {
    xy <- NULL
    xy$x <- cumsum(dis$delr)-dis$delr/2
    xy$y <- rev(cumsum(dis$delc)-dis$delc/2)
    mask[which(mask==0)] <- NA
    dis$thck <- dis$botm
    dis$thck[,,1] <- dis$TOP-dis$botm[,,1]
    for(a in 2:dis$nlay) dis$thck[,,a] <- dis$botm[,,a-1]-dis$botm[,,a]
    dis$center <- dis$botm
    for(a in 1:dis$nlay) dis$center[,,a] <- dis$botm[,,a]+dis$thck[,,a]/2
    if(is.null(i) & !is.null(j))
    {
      ids <- factor(1:(dis$nrow*dis$nlay))
      xWidth <- rep(rev(dis$delc),dis$nlay)
      yWidth <- dis$thck[,j,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$y,each=4),y=rep(dis$center[,j,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((rmodflow_array[,j,]*mask[,j,]^2)))
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
      xlabel <- 'y'
      ylabel <- 'z'
    } else if(!is.null(i) & is.null(j))
    {
      ids <- factor(1:(dis$ncol*dis$nlay))
      xWidth <- rep(dis$delr,dis$nlay)
      yWidth <- dis$thck[i,,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(dis$center[i,,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((rmodflow_array[i,,]*mask[i,,]^2)))
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
      xlabel <- 'x'
      ylabel <- 'z'
    }
    if(type=='fill') {
      if(add) {
        return(geom_polygon(aes(x=x,y=y,fill=value, group=id),data=datapoly))# +
        #scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim)) # solve this issue!
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
               geom_polygon(aes(fill=value, group=id)) +
               scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim) +
               xlab(xlabel) + ylab(ylabel))
      }
    } else if(type=='factor') {
        if(add) {
          return(geom_polygon(aes(x=x,y=y,fill=factor(value), group=id),data=datapoly))# +
          #scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim)) # solve this issue!
        } else {
          return(ggplot(datapoly, aes(x=x, y=y)) +
                   geom_polygon(aes(fill=factor(value), group=id)) +
                   scale_fill_discrete() +
                   xlab(xlabel) + ylab(ylabel))
        }
    } else if(type=='grid') {
      if(add) {
        return(geom_polygon(aes(x=x,y=y,group=id),data=datapoly,colour='black',fill=NA))# +
        #scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim)) # solve this issue!
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
               geom_polygon(aes(group=id),colour='black',fill=NA) +
               xlab(xlabel) + ylab(ylabel))
      }
    }
  }
}
