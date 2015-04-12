#' Plot a 2D section through a MODFLOW 3D array
#' 
#' \code{plot.modflow_3d_array} plots a 2D section through a MODFLOW 3D array.
#' 
#' @param modflow_3d_array An object of class modflow_3d_array
#' @param mask A 3D ibound array with 1 or TRUE indicating active cells, and 0 or F indicating inactive cells
#' @param k The number of the layer to plot
#' @param color.palette A color palette for imaging the parameter values
#' @param zlim
#' @param levels
#' @param nlevels
#' @param main
#' @return None
#' @method plot modflow_3d_array
#' @export
plot.modflow_3d_array <- function(modflow_3d_array, i=NULL, j=NULL, k=NULL, dis, mask=modflow_3d_array*0+1, zlim = range(modflow_3d_array[ifelse0(is.null(i),c(1:dim(modflow_3d_array)[1]),i),ifelse0(is.null(j),c(1:dim(modflow_3d_array)[2]),j),ifelse0(is.null(k),c(1:dim(modflow_3d_array)[3]),k)], finite=TRUE), color.palette=rev_rainbow, nlevels = 7, ...)
{
  if(!is.null(k))
  {
    modflow_2d_array <- modflow_3d_array[,,k]
    class(modflow_2d_array) <- 'modflow_2d_array'
    mask <- mask[,,k]
    plot(modflow_2d_array, dis, mask=mask, zlim=zlim, ...)
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
      return(ggplot(datapoly, aes(x=x, y=y)) +
               geom_polygon(aes(fill=value, group=id)) +
               scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim))
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
      return(ggplot(datapoly, aes(x=x, y=y)) +
               geom_polygon(aes(fill=value, group=id)) +
               scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim))
    }
  }
#   if(k=='all')
#   {
#     plot3d.modflow_2d_array(modflow_3d_array[,,1])
#     for(k in 2:dim(modflow_3d_array)[3])
#     {
#       plot3d.modflow_2d_array(modflow_3d_array[,,1],add=TRUE)
#     }
#     
#   } else {
#     plot3d.modflow_2d_array(modflow_3d_array[,,k])
#   }
}