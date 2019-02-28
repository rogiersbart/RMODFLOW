#' Create an \code{RMODFLOW} pvl object
#' 
#' \code{rmf_create_pvl} creates an \code{RMODFLOW} pvl object
#' 
#' @param np number of parameters; defaults to NULL
#' @param parnam character vector of length \code{np} specifying the parameter names; defaults to NULL
#' @param parval numeric vector of length \code{np} specifying the parameter values; defaults to NULL+
#' 
#' @return an \code{RMODFLOW} pvl object
#' @export
#' @seealso \code{\link{rmf_read_pvl}}, \code{\link{rmf_write_pvl}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?pvl.htm}

rmf_create_pvl = function(np = NULL,
                          parnam = NULL, 
                          parval = NULL){
  
  pvl = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting pvl object
  
  # data set 1
  pvl$np = np
  
  # data set 2
  pvl$parnam = parnam
  pvl$parval = parval
  
  class(pvl) = c('pvl', 'rmf_package')
  return(pvl)
  
  
}