#' Create an \code{RMODFLOW} dis object
#' 
#' \code{create_dis} creates an \code{RMODFLOW} dis object.
#' 
#' @param NLAY number of layers; defaults to 3
#' @param NROW number of rows; defaults to 10
#' @param NCOL number of columns; defaults to 10
#' @param NPER number of stress periods; defaults to 1
#' @param ITMUNI time unit; defaults to 0 (undefined)
#' @param LENUNI length unit; defaults to 0 (undefined)
#' @param LAYCBD vector of quasi-3D confining bed flags; defaults to 0 for all layers
#' @param DELR vector of cell widths along rows; defaults to 100 for all columns
#' @param DELC vector of cell widths along columns; defaults to 100 for all rows
#' @param TOP matrix with the top elevation of layer 1; defaults to 0 for all NROW x NCOL cells
#' @param BOTM 3D array with the bottom elevations of all layers; defaults to NLAY layers, equally spaced between 0 (top first layer) and -100 (bottom last layer)
#' @param PERLEN vector of stress period lengths 
#' @param NSTP vector of stress period time steps
#' @param TSMULT vector of successive time step length multipliers
#' @param SSTR character vector with steady state ('SS') or transient ('TS') stress period indicator
#' @return Object of class dis
#' @export
#' @seealso \code{\link{read_dis}}, \code{\link{write_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
create_dis <- function(NLAY = 3,
                       NROW = 10,
                       NCOL = 10,
                       NPER = 1,
                       ITMUNI = 0,
                       LENUNI = 0,
                       LAYCBD = rep(0, NLAY),
                       DELR = rep(100, NCOL),
                       DELC = rep(100, NROW),
                       TOP = matrix(0, nrow = NROW, ncol = NCOL),
                       BOTM = array(rep(seq(0,-100,length = NLAY + 1)[2:(NLAY + 1)], each = NROW * NCOL), dim = c(NROW, NCOL, NLAY)),
                       PERLEN = rep(1, NPER),
                       NSTP = rep(1, NPER),
                       TSMULT = rep(1, NPER),
                       SSTR = c('SS', rep('TS', NPER - 1))) {
  dis <- NULL
  
  # Data set 0
    #comments <- get_comments_from_lines(dis.lines)
    
  # Data set 1
    dis$NLAY <- NLAY
    dis$NROW <- NROW
    dis$NCOL <- NCOL
    dis$NPER <- NPER
    dis$ITMUNI <- ITMUNI
    dis$LENUNI <- LENUNI
  
  # Data set 2
    dis$LAYCBD <- LAYCBD
  
  # Data set 3
    dis$DELR <- DELR
  # Data set 4
    dis$DELC <- DELC
  
  # Data set 5
    dis$TOP <- TOP
    class(dis$TOP) <- 'modflow_2d_array'
  
  # Data set 6
    dis$BOTM <- BOTM
    class(dis$BOTM) <- 'modflow_3d_array'

  # Data set 7
    dis$PERLEN <- PERLEN
    dis$NSTP <- NSTP
    dis$TSMULT <- TSMULT
    dis$SSTR <- SSTR
  
  #comment(dis) <- comments
  class(dis) <- c('dis','modflow_package')
  return(dis)
}
