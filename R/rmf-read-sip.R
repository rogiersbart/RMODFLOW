#' Read a MODFLOW strongly implicit procedure file
#' 
#' \code{rmf_read_sip} reads in a MODFLOW strongly implicit procedure file and returns it as an \code{RMODFLOW} sip object
#' 
#' @param file filename; typically '*_sip'
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @return \code{RMODFLOW} sip object
#' @importFrom  readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_sip}}, \code{\link{rmf_create_sip}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?sip.htm}

rmf_read_sip = function(file = {cat('Please select strongly implicit procedure file ...\n'); file.choose()}, ...){
  
  sip = list()
  sip_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(sip_lines)
  comment(sip) = data_set_0$comments
  sip_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 =rmfi_parse_variables(sip_lines, ...)
  sip$mxiter = rmfi_ifelse0(is.na(data_set_1$variables[1]), 0, as.numeric(data_set_1$variables[1]))
  sip$nparm = rmfi_ifelse0(is.na(data_set_1$variables[2]), 0, as.numeric(data_set_1$variables[2]))
  sip_lines = data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(sip_lines, ...)
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