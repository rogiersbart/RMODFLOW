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
plot.mf3darray <- function(mf3darray, ibound=mf3darray*0+1, layer=1, color.palette=terrain.colors, zlim = range(mf3darray[,,layer], finite=TRUE), levels = pretty(zlim, nlevels), nlevels = 20, main=paste('mf3darray plot, layer',layer))
{
  mf2darray <- mf3darray[,,layer]
  class(mf2darray) <- 'mf2darray'
  ibound <- ibound[,,layer]
  plot(mf2darray, ibound, color.palette, zlim, levels, nlevels, main)
}
# plot.mf2darray <- function(mf2darray, mfw)
# {
#   
# }
# plot.mf3darray <- function(mf2darray, mfw)
# {
#   
# }