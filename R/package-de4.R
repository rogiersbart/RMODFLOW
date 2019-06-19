#' Create an \code{RMODFLOW} de4 object.
#' 
#' \code{rmf_create_de4} creates an \code{RMODFLOW} de4 object
#' 
#' @param itmx the maximum number of iterations in each time step; defaults to 100
#' @param mxup the maximum number of equations in the upper part of the equations to be solved; defaults to 0
#' @param mxlow the maximum number of equations in the lower part of the equations to be solved; defaults to 0
#' @param mxbw the maximum band width plus 1 of the AL matrix; defaults to 0
#' @param ifreq a flag indicating the frequency at which the coefficients in A change; defaults to 3
#' @param mutd4 a flag indicating the quantity of convergence information that is printed during a time step; defaults to 0
#' @param accl a multiplier for the computed head change for each iteration; defaults to 1
#' @param hclose the head change closure criterion in the unit of length; defaults to 0.01
#' @param iprd4 the time step interval for printing out convergence information when iterating; defaults to 1
#' 
#' @return \code{RMODFLOW} de4 object
#' @export
#' @seealso \code{\link{rmf_read_de4}}, \code{\link{rmf_write_de4}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?de4.htm}

rmf_create_de4 = function(itmx = 100,
                          mxup = 0, 
                          mxlow = 0,
                          mxbw = 0,
                          ifreq = 3,
                          mutd4 = 0,
                          accl = 1,
                          hclose = 0.01,
                          iprd4 = 1
                          ){
  
  de4 = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting de4 object
  
  # data set 1
 de4$itmx = itmx
 de4$mxup = mxup
 de4$mxlow = mxlow
 de4$mxbw = mxbw
  
  # data set 2
 de4$ifreq = ifreq
 de4$mutd4 = mutd4
 de4$accl = accl
 de4$hclose = hclose
 de4$iprd4 = iprd4
 
 
  class(de4) = c('de4', 'rmf_package')
  return(de4)
  
}

#' Read a MODFLOW direct solver file
#' 
#' \code{rmf_read_de4} reads in a MODFLOW direct solver file and returns it as an \code{RMODFLOW} de4 object
#' 
#' @param file filename; typically '*_de4'
#' 
#' @return \code{RMODFLOW} de4 object
#' @export
#' @seealso \code{\link{rmf_write_de4}}, \code{\link{rmf_create_de4}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?de4.htm}

rmf_read_de4 = function(file = {cat('Please select direct solver file ...\n'); file.choose()}){
  
  de4 = list()
  de4_lines = readr::read_lines(file)
  
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

#' Write a MODFLOW direct solver package
#' 
#' \code{rmf_write_de4} writes a MODFLOW direct solver file based on an \code{RMODFLOW} de4 object
#' 
#' @param de4 an \code{RMODFLOW} de4 object
#' @param file filename to write to; typically '*.de4'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_de4}}, \code{\link{rmf_create_de4}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?de4.htm}

rmf_write_de4 = function(de4, file={cat('Please choose de4 file to overwrite or provide new filename ...\n'); file.choose()} ){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Direct Solver Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(de4)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(de4$itmx, de4$mxup, de4$mxlow, de4$mxbw, file=file)
  
  # data set 2
  rmfi_write_variables(de4$ifreq, de4$mutd4, de4$accl, de4$hclose, de4$iprd4, file=file)
  
}