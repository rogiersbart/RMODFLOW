#' Create an \code{RMODFLOW} bas object
#' 
#' \code{rmf_create_bas} creates an \code{RMODFLOW} bas object.
#' 
#' @param dis RMODFLOW dis object
#' @param xsection logical; is the model a 1-row cross-section?
#' @param chtoch logical; should flow between adjacent constant-head cells be calculated?
#' @param free logical; is free format used?
#' @param printtime logical; should start, end and elapsed times be written to the global output file?
#' @param showprogress logical; should progress information be displayed?
#' @param stoperror logical; should the model be stopped based on budget percent discrepancy?
#' @param stoper numeric; threshold budget percent discrepancy
#' @param ibound 3d array specifying active (1), inactive (0) or constant head (0) status of all cells
#' @param strt 3d array specifying starting heads
#' @return Object of class bas
#' @export
#' @seealso \code{\link{rmf_read_bas}}, \code{\link{rmf_write_bas}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?bas.htm}
rmf_create_bas <- function(dis = rmf_create_dis(),
                           xsection = FALSE,
                           chtoch = FALSE,
                           free = TRUE,
                           printtime = FALSE,
                           showprogress = FALSE,
                           stoperror = FALSE,
                           stoper = 1,
                           ibound = rmf_create_array(1L, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           hnoflo = -999,
                           strt = rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))) {
      
  bas <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting bas object
  
  # data set 1
    bas$xsection <- xsection
    if(bas$xsection) warning('XSECTION: assuming ibound and strt arrays are of dimensions NLAY x NCOL')
    bas$chtoch <- chtoch
    bas$free <- free
    bas$printtime <- printtime
    bas$showprogress <- showprogress
    bas$stoperror <- stoperror
    bas$stoper <- stoper
    
  # data set 2
    bas$ibound <- rmf_create_array(apply(ibound, MARGIN = 1:length(dim(ibound)), function(i) as.integer(i)),
                                   dim = rmfi_ifelse0(bas$xsection, c(dis$nlay, dis$ncol), c(dis$nrow, dis$ncol, dis$nlay)))
  
  # data set 3
    bas$hnoflo <- hnoflo
  
  # data set 4
    bas$strt <- rmf_create_array(strt, dim = rmfi_ifelse0(bas$xsection, c(dis$nlay, dis$ncol), c(dis$nrow, dis$ncol, dis$nlay)))
  
  class(bas) <- c('bas','rmf_package')
  return(bas)
}

#' @describeIn rmf_create_bas Deprecated function name
#' @export
create_bas <- function(...) {
  .Deprecated(new = "rmf_create_bas", old = "create_bas")
  rmf_create_bas(...)
}
