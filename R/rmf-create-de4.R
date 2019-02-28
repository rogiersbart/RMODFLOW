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