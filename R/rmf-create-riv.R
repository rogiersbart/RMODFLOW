#' Create an \code{RMODFLOW} riv object.
#' 
#' \code{rmf_create_riv} creates an \code{RMODFLOW} riv object
#' 
#' @param npriv number of riv parameters; defaults to 1
#' @param mxl maximum number of river reaches that will be defined using parameters; defaults to 10
#' @param mxactr maximum number of river reaches in use during any stress period defined with and without parameters; defaults to 10
#' @param irivcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param option optional character vector specifying the auxiliary variables and/or \code{NOPRINT}. Do not include the keywords \code{AUXILIARY} or \code{AUX}; defaults to NULL
#' @param parnam vector of length \code{npriv} specifying the parameter names; defaults to 'STREAM'
#' @param parval vector of length \code{npriv} specifying the parameter values; defaults to 1
#' @param nlst vector of length \code{npriv} specifying the number of river reaches that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to 10
#' @param instances logical vector of length \code{npriv} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{npriv} indicating the number instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{npriv} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param layer_parm list with \code{npriv} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the layer numbers of the model cells containing the river reaches specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 1 for all river reaches for the parameter.
#' @param row_parm list with \code{npriv} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the row numbers of the model cells containing the river reaches specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 1 for all river reaches for the parameter
#' @param column_parm list with \code{npriv} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the column numbers of the model cells containing the river reaches specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 1:10 for the parameter
#' @param stage_parm list with \code{npriv} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the head in the river for parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to -2 for all river reaches for the parameter
#' @param condfact_parm list with \code{npriv} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the river conductance factor for parameter \code{i}. The river conductance is the product of condfact and the parameter value. If not time-varying, set numinst dimension to 1. Defaults to 5e-7 for all river reaches for the parameter
#' @param rbot_parm list with \code{npriv} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the elevation of the riverbed bottom for parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to -4 for all river reaches for the parameter
#' @param xyz_parm optional list with \code{npriv} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nlst} specifying the auxiliary variables for parameter \code{i}. If not time-varying, set numinst to 1. Defaults to NULL
#' @param itmp vector of length \code{dis$nper} specifying non-parameter river data being read for each stress period; defaults to 0
#' @param np vector of length \code{dis$nper} specifying the number of river parameters in use for each stress period; defaults to 1
#' @param layer_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the layer numbers of the model cells containing the river reaches; defaults to NULL
#' @param row_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the row numbers of the model cells containing the river reaches; defaults to NULL
#' @param column_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the column numbers of the model cells containing the river reaches; defaults to NULL
#' @param stage_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the river stages of the model cells containing the river reaches; defaults to NULL
#' @param cond_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the riverbed conductance of the model cells containing the river reaches; defaults to NULL
#' @param rbot_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the bottom elevations of the riverbed in the model cells containing the river reaches; defaults to NULL
#' @param xyz_sp optional list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{itmp} for stress period \code{i} specifying the auxiliary variables of the model cells containing the river reaches; defaults to NULL
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameters being used; defaults to 'STREAM'
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' 
#' @return \code{RMODFLOW} riv object
#' @export
#' @seealso \code{\link{rmf_read_riv}}, \code{\link{rmf_write_riv}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?riv.htm}

rmf_create_riv = function(npriv = 1,
                          mxl = 10,
                          mxactr = 10,
                          irivcb = 0,
                          option = NULL,
                          parnam = 'STREAM', 
                          parval = 1,
                          nlst = 10, 
                          instances = NULL,
                          numinst = NULL, 
                          instnam = NULL,
                          layer_parm = list(array(rep(1, 10), dim=c(1, 10))),
                          row_parm = list(array(rep(1, 10), dim=c(1, 10))),
                          column_parm = list(array(1:10, dim=c(1, 10))),
                          stage_parm =  list(array(-2, dim=c(1, 10))),
                          condfact_parm =  list(array(5E-7, dim=c(1, 10))),
                          rbot_parm =  list(array(-4, dim=c(1, 10))),
                          xyz_parm = NULL,
                          itmp = 0,
                          np = 1,
                          layer_sp = NULL,
                          row_sp = NULL,
                          column_sp = NULL,
                          stage_sp = NULL,
                          cond_sp = NULL,
                          rbot_sp = NULL,
                          xyz_sp = NULL, 
                          pname = list('STREAM'),
                          iname = NULL
){
  
  riv = list()
  
  
  # data set 0
  # to provide comments, use ?comment on resulting riv object
  
  # data set 1
  if (!is.null(npriv)){
    riv$npriv = npriv
    riv$mxl = mxl
  }
  
  
  # data set 2
  riv$mxactr = mxactr
  riv$irivcb = irivcb
  if(!is.null(option)) riv$option = option
  
  if(!is.null(riv$npriv) && riv$npriv > 0){
    
    # data set 3
    riv$parnam = parnam
    riv$partyp = rep('RIV', riv$npriv)
    riv$parval = parval
    riv$nlst = nlst
    if(!is.null(instances) && T %in% instances) riv$instances = instances
    if(!is.null(riv$instances)) riv$numinst = numinst
    
    # data set 4a
    if(!is.null(riv$instances) && T %in% riv$instances) riv$instnam = instnam
    
    # data set 4b
    riv$layer_parm = layer_parm
    riv$row_parm = row_parm
    riv$column_parm = column_parm
    riv$stage_parm = stage_parm
    riv$condfact_parm = condfact_parm
    riv$rbot_parm = rbot_parm
    if(!is.null(xyz_parm) && !is.null(riv$option) && (riv$option != "NOPRINT")) riv$xyz_parm = xyz_parm
    
  }
  
  # data set 5
  riv$itmp = itmp
  riv$np = np
  
  # data set 6
  if(any(riv$itmp > 0)){
    
    riv$layer_sp = layer_sp
    riv$row_sp = row_sp
    riv$column_sp = column_sp
    riv$stage_sp = stage_sp
    riv$cond_sp = cond_sp
    riv$rbot_sp = rbot_sp
    if(!is.null(xyz_sp) && !is.null(riv$option) && (riv$option != "NOPRINT")) riv$xyz_sp = xyz_sp
    
  }
  
  # data set 7
  if(any(riv$np > 0)){
    
    riv$pname = pname
    if(!is.null(riv$instances) && T %in% riv$instances) riv$iname = iname
    
  }
  
  
  class(riv) = c('riv', 'rmf_package')
  return(riv)
  
}