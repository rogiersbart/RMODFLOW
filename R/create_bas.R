#' Create an \code{RMODFLOW} bas object
#' 
#' \code{create_bas} creates an \code{RMODFLOW} bas object.
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
#' @seealso \code{\link{read_bas}}, \code{\link{write_bas}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?bas.htm}
create_bas <- function(dis = create_dis(),
                       xsection = FALSE,
                       chtoch = FALSE,
                       free = TRUE,
                       printtime = FALSE,
                       showprogress = FALSE,
                       stoperror = FALSE,
                       stoper = 1,
                       ibound = create_rmodflow_array(1, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                       hnoflo = -999,
                       strt = create_rmodflow_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))) {
  
  bas <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting bas object
  
  # data set 1
    bas$xsection <- xsection
    bas$chtoch <- chtoch
    bas$free <- free
    bas$printtime <- printtime
    bas$showprogress <- showprogress
    bas$stoperror <- stoperror
    bas$stoper <- stoper
    
  # data set 2
    bas$ibound <- ibound
  
  # data set 3
    bas$hnoflo <- hnoflo
  
  # data set 4
    bas$strt <- strt
  
  class(bas) <- c('bas','modflow_package')
  return(bas)
}
