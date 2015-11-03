#' Plot a 2D section through a MODFLOW 3D array
#' 
#' \code{plot.modflow_3d_array} plots a 2D section through a MODFLOW 3D array.
#' 
#' @param modflow_3d_array an object of class modflow_3d_array
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param dis discretization file object
#' @param ba6 basic file object; optional
#' @param mask a 3D array with 0 or F indicating inactive cells optional; defaults to having all cells active or, if ba6 is provided, ba6$IBOUND
#' @param colour_palette a colour palette for imaging the array values
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param ... parameters provided to plot.modflow_2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method plot modflow_3d_array
#' @export
plot.modflow_3d_array <- function(modflow_3d_array, i=NULL, j=NULL, k=NULL, dis, ba6=NULL, mask=ifelse0(is.null(ba6),modflow_3d_array*0+1,ba6$IBOUND), zlim = range(modflow_3d_array[ifelse0(is.null(i),c(1:dim(modflow_3d_array)[1]),i),ifelse0(is.null(j),c(1:dim(modflow_3d_array)[2]),j),ifelse0(is.null(k),c(1:dim(modflow_3d_array)[3]),k)], finite=TRUE), colour_palette=rev_rainbow, nlevels = 7, type='fill', add=FALSE, ...)
{
  if(!is.null(k))
  {
    modflow_2d_array <- modflow_3d_array[,,k]
    class(modflow_2d_array) <- 'modflow_2d_array'
    mask <- mask[,,k]
    plot(modflow_2d_array, dis, mask=mask, zlim=zlim, type=type, add=add, ...)
  } else {
    xy <- NULL
    xy$x <- cumsum(dis$DELR)-dis$DELR/2
    xy$y <- (cumsum(dis$DELC)-dis$DELC/2)
    mask[which(mask==0)] <- NA
    dis$THCK <- dis$BOTM
    dis$THCK[,,1] <- dis$TOP-dis$BOTM[,,1]
    for(a in 2:dis$NLAY) dis$THCK[,,a] <- dis$BOTM[,,a-1]-dis$BOTM[,,a]
    dis$CENTER <- dis$BOTM
    for(a in 1:dis$NLAY) dis$CENTER[,,a] <- dis$BOTM[,,a]+dis$THCK[,,a]/2
    if(is.null(i) & !is.null(j))
    {
      ids <- factor(1:(dis$NROW*dis$NLAY))
      xWidth <- rep(dis$DELC,dis$NLAY)
      yWidth <- dis$THCK[,j,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$y,each=4),y=rep(dis$CENTER[,j,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((modflow_3d_array[,j,]*mask[,j,]^2)))
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
    } else if(!is.null(i) & is.null(j))
    {
      ids <- factor(1:(dis$NCOL*dis$NLAY))
      xWidth <- rep(dis$DELR,dis$NLAY)
      yWidth <- dis$THCK[i,,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(dis$CENTER[i,,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((modflow_3d_array[i,,]*mask[i,,]^2)))
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
    }
    if(type=='fill') {
      if(add) {
        return(geom_polygon(aes(x=x,y=y,fill=value, group=id),data=datapoly))# +
        #scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim)) # solve this issue!
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
               geom_polygon(aes(fill=value, group=id)) +
               scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim))
      }
    } else if(type=='grid') {
      if(add) {
        return(geom_polygon(aes(x=x,y=y,group=id),data=datapoly,colour='black',fill=NA))# +
        #scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim)) # solve this issue!
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
               geom_polygon(aes(group=id),colour='black',fill=NA))
      }
    }
  }
}
