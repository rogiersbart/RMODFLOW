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

#' Read a MODFLOW strongly implicit procedure file
#' 
#' \code{rmf_read_sip} reads in a MODFLOW strongly implicit procedure file and returns it as an \code{RMODFLOW} sip object
#' 
#' @param file filename; typically '*_sip'
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @return \code{RMODFLOW} sip object
#' @export
#' @seealso \code{\link{rmf_write_sip}}, \code{\link{rmf_create_sip}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?sip.htm}

rmf_read_sip = function(file = {cat('Please select strongly implicit procedure file ...\n'); file.choose()}, ...){
  
  sip = list()
  sip_lines = readr::read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(sip_lines)
  comment(sip) = data_set_0$comments
  sip_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 =rmfi_parse_variables(sip_lines, n = 2, ...)
  sip$mxiter = rmfi_ifelse0(is.na(data_set_1$variables[1]), 0, as.numeric(data_set_1$variables[1]))
  sip$nparm = rmfi_ifelse0(is.na(data_set_1$variables[2]), 0, as.numeric(data_set_1$variables[2]))
  sip_lines = data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(sip_lines, n = 5, ...)
  sip$accl = rmfi_ifelse0(is.na(data_set_2$variables[1]), 0, as.numeric(data_set_2$variables[1]))
  sip$hclose = rmfi_ifelse0(is.na(data_set_2$variables[2]), 0, as.numeric(data_set_2$variables[2]))
  sip$ipcalc = rmfi_ifelse0(is.na(data_set_2$variables[3]), 0, as.numeric(data_set_2$variables[3]))
  sip$wseed = rmfi_ifelse0(is.na(data_set_2$variables[4]), 0, as.numeric(data_set_2$variables[4]))
  sip$iprsip = rmfi_ifelse0(is.na(data_set_2$variables[5]), 0, as.numeric(data_set_2$variables[5]))
  sip_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  class(sip) = c('sip', 'rmf_package')
  return(sip)
  
  
}

#' Write a MODFLOW strongly implicit procedure package
#' 
#' \code{rmf_write_sip} writes a MODFLOW strongly implicit procedure file based on an \code{RMODFLOW} sip object
#' 
#' @param sip an \code{RMODFLOW} sip object
#' @param file filename to write to; typically '*.sip'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_sip}}, \code{\link{rmf_create_sip}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?sip.htm}

rmf_write_sip = function(sip, file={cat('Please choose sip file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Strongly Implicit Procedure Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(sip)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(sip$mxiter, sip$nparm, file=file, ...)
  
  # data set 2
  rmfi_write_variables(sip$accl, sip$hclose, sip$ipcalc, sip$wseed, sip$iprsip, file=file, ...)
  
}
