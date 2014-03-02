##### Functions for geostatistical mapping of upscaled 2D fields
model.2d.variogram <- function(dat, model='Exp')
{
  vgm.hor <- NULL; expvar.hor <- NULL
  gstatobject <- gstat(id='z',formula= z ~ 1, locations= ~ x+y, data=dat)
  expvar.hor <- variogram(gstatobject, width=100, beta=0, cutoff=10000)
  varmodhor <- vgm(psill=var(dat$z), model=model, range=500, nugget=var(dat$z)/10)
  vgm.hor <- fit.variogram(expvar.hor, varmodhor, fit.sills=T, fit.ranges=T)
  plot(expvar.hor$dist, expvar.hor$gamma, xlab='Horizontal lag distance (m)', ylab='Semivariance')
  lines(variogramLine(vgm.hor, maxdist=10000), col='red', lwd=3)
  print(vgm.hor)
  return(vgm.hor)
}
krige.2d.field <- function(x,y,attribute)
{
  dat <- remove.na.rows(data.frame(x=x, y=y, z=attribute))
  dat <- dat[which(dat$z!=Inf & dat$z!=-Inf & dat$z!='NaN'),]
  newdata <- data.frame(x=as.vector(aqmatrix$x),y=as.vector(aqmatrix$y))
  vgmModel <- model.2d.variogram(dat, model='Exp')
  dat$x <- jitter(dat$x, amount=1)
  gstatobj <- gstat(formula = z ~1, locations = ~ x + y, model=vgmModel, data=dat)
  krige <- predict.gstat(gstatobj, newdata=newdata)
  map <- matrix(data=krige$var1.pred , nrow=180, ncol=250)
  return(map)       
}
scale.mat <- function(mat) { return( (mat - mean(mat))/sd(c(mat)))}
##### Functions for upscaling hydraulic conductivity estimates to a representative value
# horizontal: arithmetic!! --> normal mean() function
# vertical: harmonic!!
harmean <- function(a){return(1/(mean(1/a, na.rm=T)))} # for vertical K averaging
geomean <- function(a){return(prod(a)^(1/length(a)))}