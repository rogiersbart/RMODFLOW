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
plot.mf2darray <- function(mf2darray, ibound=mf2darray*0+1, color.palette=terrain.colors, zlim = range(mf2darray, finite=TRUE), levels = pretty(zlim, nlevels), nlevels = 20, main='MF2DARRAY plot')
{
  nr <- nrow(mf2darray)
  nc <- ncol(mf2darray)
  ibound[which(ibound==0)] <- NA
  mf2darray[which(mf2darray < zlim[1])] <- zlim[1]
  mf2darray[which(mf2darray > zlim[2])] <- zlim[2]
  filled.contour(t(mirror.matrix(mf2darray, 'vertical'))*t(mirror.matrix(ibound, 'vertical')),
                 plot.title=title(main=main, xlab='X (km)', ylab='Y (km)'),
                 color.palette=color.palette, asp=nr/nc, levels=levels, zlim=zlim,
                 plot.axes = { axis(1, seq(0, 1, by = 1/12.5),labels=seq(0,nc*50/1000,nc*50/1000/12.5))
                               axis(2, seq(0, 1, by = 1/9), labels=seq(0,nr*50/1000,nr*50/1000/9)) }) #, zlim=c(-2,2)
}