#' Read a MODFLOW direct solver file
#' 
#' \code{rmf_read_de4} reads in a MODFLOW direct solver file and returns it as an \code{RMODFLOW} de4 object
#' 
#' @param file filename; typically '*_de4'
#' 
#' @return \code{RMODFLOW} de4 object
#' @importFrom  readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_de4}}, \code{\link{rmf_create_de4}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?de4.htm}

rmf_read_de4 = function(file = {cat('Please select direct solver file ...\n'); file.choose()}){
  
  de4 = list()
  de4_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(de4_lines)
  comment(de4) = data_set_0$comments
  de4_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(de4_lines)
  de4$itmx = data_set_1$variables[1]
  de4$mxup = data_set_1$variables[2]
  de4$mxlow = data_set_1$variables[3]
  de4$mxbw = data_set_1$variables[4]
  de4_lines = data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(de4_lines)
  de4$ifreq = data_set_2$variables[1]
  de4$mutd4 = data_set_2$variables[2]
  de4$accl = data_set_2$variables[3]
  de4$hclose = data_set_2$variables[4]
  de4$iprd4 = data_set_2$variables[5]
  de4_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  class(de4) = c('de4', 'rmf_package')
  return(de4)
}