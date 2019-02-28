#' Create an \code{RMODFLOW} mlt object
#' 
#' \code{rmf_create_mlt} creates an \code{RMODFLOW} mlt object
#' 
#' @param nml number of multiplier arrays to be defined; defaults to 1
#' @param mltnam character vector of length \code{nml} specifying the names of the multiplier arrays; defaults to 'MULT'
#' @param functn optional logical vector of length \code{nml} indicating if the multiplier array will be constructed from other multiplier arrays previously defined; defaults to NULL
#' @param rmlt list with \code{nml} elements where each element is a \code{rmf_2d_array} specifying a mutliplier array; defaults to a \code{rmf_2d_array} with 1 for all cells
#' @param operators list with \code{nml} elements where each element is a character vector with the correct function which will be printed for that multiplier array. If no function is to be specifyied for an array, set to NULL; defaults to NULL
#' @param iprn numeric vector of length \code{nml} indicating the printing format and whether the multiplier array constructed in data set 4 will be printed to the listing file; defaults to NULL
#' 
#' @return an \code{RMODFLOW} mlt object
#' @export
#' @seealso \code{\link{rmf_read_mlt}}, \code{\link{rmf_write_mlt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?mult.htm}

rmf_create_mlt <- function(nml = 1,
                          mltnam = 'MULT',
                          functn = NULL, 
                          rmlt = list(rmf_create_array(1.0, dim=c(10, 10))),
                          operators = NULL,
                          iprn = NULL
                          ){
  
  mlt <- list()
  
  # data set 0
  # to provide comments, use ?comment on resulting mlt object
  
  # data set 1
  mlt$nml <-  nml
  
  # data set 2
  mlt$mltnam <-  mltnam
  if(!is.null(functn) && (T %in% functn)) mlt$functn <-  functn

  # data set 3
  if(is.null(mlt$functn) || (!is.null(mlt$functn) && (F %in% mlt$functn))) mlt$rmlt <-  rmlt
  
  # data set 4
  if(!is.null(mlt$functn) && (T %in% mlt$functn)) {
    mlt$operators <-  operators
    mlt$iprn <-  iprn
  }
  
  class(mlt) <-  c('mlt', 'modflow_package')
  return(mlt)
  
}