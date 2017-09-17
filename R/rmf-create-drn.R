#' Create an \code{RMODFLOW} drn object.
#' 
#' \code{rmf_create_drn} creates an \code{RMODFLOW} drn object
#' 
#' @param npdrn number of drn parameters; defaults to 1
#' @param mxl maximum number of drain cells that will be defined using parameters; defaults to 10
#' @param mxactd maximum number of drain cells in use during any stress period defined with and without parameters; defaults to 10
#' @param idrncb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param option optional character vector specifying the auxiliary variables and/or \code{NOPRINT}. Do not include the keywords \code{AUXILIARY} or \code{AUX}; defaults to NULL
#' @param parnam vector of length \code{npdrn} specifying the parameter names; defaults to 'DRAIN'
#' @param parval vector of length \code{npdrn} specifying the parameter values; defaults to 1
#' @param nlst vector of length \code{npdrn} specifying the number of drain cells that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to 10
#' @param instances logical vector of length \code{npdrn} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{npdrn} indicating the number instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{npdrn} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param layer_parm list with \code{npdrn} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the layer numbers of the model cells containing the drains specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 1 for all drain cells for the parameter.
#' @param row_parm list with \code{npdrn} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the row numbers of the model cells containing the drains specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 10 for all drain cells for the parameter
#' @param column_parm list with \code{npdrn} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the column numbers of the model cells containing the drains specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 1:10 for the parameter
#' @param elevation_parm list with \code{npdrn} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the elevation of the drain for parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to -1 for all drain cells for the parameter
#' @param condfact_parm list with \code{npdrn} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the drain conductance factor for parameter \code{i}. The drain conductance is the product of condfact and the parameter value. If not time-varying, set numinst dimension to 1. Defaults to 5e-6 for all drain cells for the parameter
#' @param xyz_parm optional list with \code{npdrn} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nlst} specifying the auxiliary variables for parameter \code{i}. If not time-varying, set numinst to 1. Defaults to NULL
#' @param itmp vector of length \code{dis$nper} specifying non-parameter drain data being read for each stress period; defaults to 0
#' @param np vector of length \code{dis$nper} specifying the number of drain parameters in use for each stress period; defaults to 1
#' @param layer_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the layer numbers of the model cells containing the drains; defaults to NULL
#' @param row_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the row numbers of the model cells containing the drains; defaults to NULL
#' @param column_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the column numbers of the model cells containing the drains; defaults to NULL
#' @param elevation_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the drain elevations of the model cells containing the drains; defaults to NULL
#' @param cond_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the drain conductance of the model cells containing the drains; defaults to NULL
#' @param xyz_sp optional list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{itmp} for stress period \code{i} specifying the auxiliary variables of the model cells containing the drains; defaults to NULL
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameters being used; defaults to 'DRAIN'
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' 
#' @return \code{RMODFLOW} drn object
#' @export
#' @seealso \code{\link{rmf_read_drn}}, \code{\link{rmf_write_drn}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?drn.htm}

rmf_create_drn = function(npdrn = 1,
                          mxl = 10,
                          mxactd = 10,
                          idrncb = 0,
                          option = NULL,
                          parnam = 'DRAIN', 
                          parval = 1,
                          nlst = 10, 
                          instances = NULL,
                          numinst = NULL, 
                          instnam = NULL,
                          layer_parm = list(array(rep(1, 10), dim=c(1, 10))),
                          row_parm = list(array(rep(10, 10), dim=c(1, 10))),
                          column_parm = list(array(1:10, dim=c(1, 10))),
                          elevation_parm =  list(array(-1, dim=c(1, 10))),
                          condfact_parm =  list(array(5E-6, dim=c(1, 10))),
                          xyz_parm = NULL,
                          itmp = 0,
                          np = 1,
                          layer_sp = NULL, 
                          row_sp = NULL,
                          column_sp = NULL,
                          elevation_sp = NULL,
                          cond_sp = NULL,
                          xyz_sp = NULL, 
                          pname = list('DRAIN'),
                          iname = NULL
){
  
  drn = list()
  
  
  # data set 0
  # to provide comments, use ?comment on resulting drn object
  
  # data set 1
  if (!is.null(npdrn)){
    drn$npdrn = npdrn
    drn$mxl = mxl
  }
  
  
  # data set 2
  drn$mxactd = mxactd
  drn$idrncb = idrncb
  if(!is.null(option)) drn$option = option
  
  if(!is.null(drn$npdrn) && drn$npdrn > 0){
    
    # data set 3
    drn$parnam = parnam
    drn$partyp = rep('DRN', drn$npdrn)
    drn$parval = parval
    drn$nlst = nlst
    if(!is.null(instances) && T %in% instances) drn$instances = instances
    if(!is.null(drn$instances)) drn$numinst = numinst
    
    # data set 4a
    if(!is.null(drn$instances) && T %in% drn$instances) drn$instnam = instnam
    
    # data set 4b
    drn$layer_parm = layer_parm
    drn$row_parm = row_parm
    drn$column_parm = column_parm
    drn$elevation_parm = elevation_parm
    drn$condfact_parm = condfact_parm
    if(!is.null(xyz_parm) && !is.null(drn$option) && (drn$option != "NOPRINT")) drn$xyz_parm = xyz_parm
    
  }
  
  # data set 5
  drn$itmp = itmp
  drn$np = np
  
  # data set 6
  if(any(drn$itmp > 0)){
    
    drn$layer_sp = layer_sp
    drn$row_sp = row_sp
    drn$column_sp = column_sp
    drn$elevation_sp = elevation_sp
    drn$cond_sp = cond_sp
    if(!is.null(xyz_sp) && !is.null(drn$option) && (drn$option != "NOPRINT")) drn$xyz_sp = xyz_sp
    
  }
  
  # data set 7
  if(any(drn$np > 0)){
    
    drn$pname = pname
    if(!is.null(drn$instances) && T %in% drn$instances) drn$iname = iname
    
  }
  
  
  class(drn) = c('drn', 'rmf_package')
  return(drn)
  
}