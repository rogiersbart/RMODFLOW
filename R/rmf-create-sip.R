#' Create an \code{RMODFLOW} sip object.
#' 
#' \code{rmf_create_sip} creates an \code{RMODFLOW} sip object
#' 
#' @param mxiter maximum number of iterations in one time step attempting to solve the equations; defaults to 50
#' @param nparm number of iteration variables; defaults to 5
#' @param accl acceleration variable; defaults to 1
#' @param hclose head change criterion for convergence in units of length; defaults to 0.01
#' @param ipcalc flag indicating where the seed for calculating iteration variables comes from; defaults to 1
#' @param wseed user-specified seed for calculating iteration variables; defaults to 0
#' @param iprsip printout interval for maximum head change; defaults to 0
#' 
#' @return \code{RMODFLOW} sip object
#' @export
#' @seealso \code{\link{rmf_read_sip}}, \code{\link{rmf_write_sip}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?sip.htm}

rmf_create_sip = function(mxiter = 50,
                          nparm = 5,
                          accl = 1,
                          hclose = 0.01,
                          ipcalc = 1,
                          wseed = 0,
                          iprsip = 0
                          ){
  
  sip = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting sip object
  
  # data set 1
  sip$mxiter = mxiter
  sip$nparm = nparm
  
  # data set 2
  sip$accl = accl
  sip$hclose = hclose
  sip$ipcalc = ipcalc
  sip$wseed = wseed
  sip$iprsip = iprsip
  
  class(sip) = c('sip', 'rmf_package')
  return(sip)
  
}