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
#' @import ggplot2 directlabels
plot.mf2darray <- function(mf2darray, dis, ibound=mf2darray*0+1, color.palette=terrain.colors, zlim = range(mf2darray, finite=TRUE), levels = pretty(zlim, nlevels), nlevels = 20, main='MF2DARRAY plot')
{
  ids <- factor(1:(dis$NROW*dis$NCOL))
  ibound[which(ibound==0)] <- NA
  xWidth <- rep(dis$DELR,dis$NROW)
  yWidth <- rep(dis$DELC,each=dis$NCOL)
  xy <- expand.grid(cumsum(dis$DELR)-dis$DELR/2,sum(dis$DELC)-(cumsum(dis$DELC)-dis$DELC/2))
  names(xy) <- c('x','y')
  positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(xy$y,each=4))
  positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
  positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
  positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
  positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
  positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
  positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
  positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
  positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
  values <- data.frame(id = ids,value = c(t(mf2darray*ibound)))
  datapoly <- merge(values, positions, by=c("id"))
  ggplot(datapoly, aes(x=x, y=y)) +
    geom_polygon(aes(fill=value, group=id)) +
    scale_fill_gradientn(colours=rainbow(7))
}