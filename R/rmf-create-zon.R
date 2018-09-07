#' Create an \code{RMODFLOW} zon object
#' 
#' \code{rmf_create_zon} creates an \code{RMODFLOW} zon object
#' 
#' @param nzn number of zone arrays to be defined; defaults to 1
#' @param zonnam character vector of length \code{nzn} specifying the names of zone arrays; defaults to 'ZONE'
#' @param izon numeric 3D array of dimensions \code{dis$nrow x dis$ncol x nzn} specifying the zone arrays; defaults to 1 for all cells
#'
#' @return an \code{RMODFLOW} zon object
#' @export
#' @seealso \code{\link{rmf_read_zon}}, \code{\link{rmf_write_zon}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?zone.htm}

rmf_create_zon = function(nzn = 1,
                      zonnam = 'ZONE',
                      izon = array(1L, dim=c(10, 10, 1))
                      ){
  
  zon <-  list()
  
  # data set 
  # to provide comments, use ?comment on resulting zon object
  
  # data set 1
  zon$nzn <-  nzn
  
  # data set 2
  zon$zonnam <-  zonnam
  
  # data set 3
  zon$izon <-  rmf_create_array(apply(izon, MARGIN = 1:length(dim(izon)), function(i) as.integer(i)))
  
  class(zon) <-  c('zon', 'modflow_package')
  return(zon)
  
}