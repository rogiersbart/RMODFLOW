#' Create an \code{RMODFLOW} dis object
#' 
#' \code{rmf_create_dis} creates an \code{RMODFLOW} dis object.
#' 
#' @param nlay number of layers; defaults to 3
#' @param nrow number of rows; defaults to 10
#' @param ncol number of columns; defaults to 10
#' @param nper number of stress periods; defaults to 1
#' @param itmuni time unit; defaults to 1 (seconds)
#' @param lenuni length unit; defaults to 2 (metres)
#' @param laycbd vector of quasi-3D confining bed flags; defaults to 0 for all layers
#' @param delr vector of cell widths along rows; defaults to 100 for all columns
#' @param delc vector of cell widths along columns; defaults to 100 for all rows
#' @param top matrix with the top elevation of layer 1; defaults to 0 for all nrow x ncol cells
#' @param botm 3D array with the bottom elevations of all layers; defaults to nlay layers, equally spaced between 0 (top first layer) and -100 (bottom last layer)
#' @param perlen vector of stress period lengths 
#' @param nstp vector of stress period time steps
#' @param tsmult vector of successive time step length multipliers
#' @param sstr character vector with steady state ('SS') or transient ('TS') stress period indicator
#' @return Object of class dis
#' @export
#' @seealso \code{\link{rmf_read_dis}}, \code{\link{rmf_write_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
rmf_create_dis <- function(nlay = 3,
                           nrow = 10,
                           ncol = 10,
                           nper = 1,
                           itmuni = 1,
                           lenuni = 2,
                           laycbd = rep(0, nlay),
                           delr = rep(100, ncol),
                           delc = rep(100, nrow),
                           top = matrix(0, nrow = nrow, ncol = ncol),
                           botm = array(rep(seq(0,-10 * nlay,length = nlay + 1)[2:(nlay + 1)], each = nrow * ncol), dim = c(nrow, ncol, nlay)),
                           perlen = rep(1, nper),
                           nstp = rep(1, nper),
                           tsmult = rep(1, nper),
                           sstr = c('SS', rep('TS', nper - 1))) {
  dis <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting dis object
    
  # data set 1
    dis$nlay <- nlay
    dis$nrow <- nrow
    dis$ncol <- ncol
    dis$nper <- nper
    dis$itmuni <- itmuni
    dis$lenuni <- lenuni
  
  # data set 2
    dis$laycbd <- laycbd
  
  # data set 3
    dis$delr <- delr
  # data set 4
    dis$delc <- delc
  
  # data set 5
    dis$top <- top
    class(dis$top) <- 'rmf_2d_array'
  
  # data set 6
    dis$botm <- botm
    class(dis$botm) <- 'rmf_3d_array'

  # data set 7
    dis$perlen <- perlen
    dis$nstp <- nstp
    dis$tsmult <- tsmult
    dis$sstr <- sstr
  
  #comment(dis) <- comments
  class(dis) <- c('dis','rmf_package')
  return(dis)
}

#' @describeIn rmf_create_dis Deprecated function name
create_dis <- function(...) {
  .Deprecated(new = "rmf_create_dis", old = "create_dis")
  rmf_create_dis(...)
}
