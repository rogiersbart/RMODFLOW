#' Plot a layer of a MODFLOW 3D array
#' 
#' \code{plot.mf3darray} plots a layer of a MODFLOW 3D array.
#' 
#' @param mf3darray An object of class mf3darray, or a 3D array
#' @param ibound A 3D ibound array with 1 or TRUE indicating active cells, and 0 or F indicating inactive cells
#' @param layer The number of the layer to plot
#' @param color.palette A color palette for imaging the parameter values
#' @param zlim
#' @param levels
#' @param nlevels
#' @param main
#' @return None
#' @method plot mf3darray
#' @export
plot.mf3darray <- function(mf3darray, layer=1, dis, ibound=mf3darray*0+1, zlim = range(mf3darray[,,layer], finite=TRUE), ...)
{
  mf2darray <- mf3darray[,,layer]
  class(mf2darray) <- 'mf2darray'
  ibound <- ibound[,,layer]
  plot(mf2darray, dis, ibound=ibound, zlim=zlim, ...)
}