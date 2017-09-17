#' Create an \code{RMODFLOW} wel object.
#' 
#' \code{rmf_create_wel} creates an \code{RMODFLOW} wel object
#' 
#' @param npwel number of wel parameters; defaults to NULL
#' @param mxl maximum number of wells that will be defined using parameters; defaults to NULL
#' @param mxactw maximum number of wells in use during any stress period defined with and without parameters; defaults to 1
#' @param iwelcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param option optional character vector specifying the auxiliary variables and/or \code{NOPRINT}. Do not include the keywords \code{AUXILIARY} or \code{AUX}; defaults to NULL
#' @param parnam vector of length \code{npwel} specifying the parameter names; defaults to NULL
#' @param parval vector of length \code{npwel} specifying the parameter values; defaults to NULL
#' @param nlst vector of length \code{npwel} specifying the number of wells that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to NULL
#' @param instances logical vector of length \code{npwel} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{npwel} indicating the number instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{npwel} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param layer_parm list with \code{npwel} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the layer numbers of the model cells containing the wells specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param row_parm list with \code{npwel} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the row numbers of the model cells containing the wells specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param column_parm list with \code{npwel} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the column numbers of the model cells containing the wells specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param qfact_parm list with \code{npwel} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the well recharge factor for parameter \code{i}. The recharge is the product of qfact and the parameter value. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param xyz_parm optional list with \code{npwel} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nlst} specifying the auxiliary variables for parameter \code{i}. If not time-varying, set numinst to 1. Defaults to NULL
#' @param itmp vector of length \code{dis$nper} specifying non-parameter well data being read for each stress period; defaults to 1
#' @param np vector of length \code{dis$nper} specifying the number of well parameters in use for each stress period; defaults to 0
#' @param layer_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the layer numbers of the model cells containing the wells; defaults to 3
#' @param row_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the row numbers of the model cells containing the wells; defaults to 5
#' @param column_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the column numbers of the model cells containing the wells; defaults to 5
#' @param q_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the volumetric recharge rates of the model cells containing the wells; defaults to -0.002
#' @param xyz_sp optional list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{itmp} for stress period \code{i} specifying the auxiliary variables of the model cells containing the wells; defaults to NULL
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameters being used; defaults to NULL
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' 
#' @return \code{RMODFLOW} wel object
#' @export
#' @seealso \code{\link{rmf_read_wel}}, \code{\link{rmf_write_wel}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?wel.htm}

rmf_create_wel = function(npwel = NULL,
                      mxl = NULL,
                      mxactw = 1,
                      iwelcb = 0,
                      option = NULL,
                      parnam = NULL, 
                      parval = NULL,
                      nlst = NULL, 
                      instances = NULL,
                      numinst = NULL, 
                      instnam = NULL,
                      layer_parm = NULL,
                      row_parm = NULL,
                      column_parm = NULL,
                      qfact_parm = NULL,
                      xyz_parm = NULL,
                      itmp = 1,
                      np = 0,
                      layer_sp = list(c(3)), 
                      row_sp = list(c(5)), 
                      column_sp = list(c(5)), 
                      q_sp = list(c(-0.002)), 
                      xyz_sp = NULL, 
                      pname = NULL,
                      iname = NULL
){
  
  wel = list()
  
  
  # data set 0
  # to provide comments, use ?comment on resulting wel object
  
  # data set 1
  if (!is.null(npwel)){
    wel$npwel = npwel
    wel$mxl = mxl
  }

  
  # data set 2
  wel$mxactw = mxactw
  wel$iwelcb = iwelcb
  if(!is.null(option)) wel$option = option
  
  if(!is.null(wel$npwel) && wel$npwel > 0){
    
    # data set 3
    wel$parnam = parnam
    wel$partyp = rep('Q', wel$npwel)
    wel$parval = parval
    wel$nlst = nlst
    if(!is.null(instances) && T %in% instances) wel$instances = instances
    if(!is.null(wel$instances)) wel$numinst = numinst
    
    # data set 4a
    if(!is.null(wel$instances) && T %in% wel$instances) wel$instnam = instnam
    
    # data set 4b
    wel$layer_parm = layer_parm
    wel$row_parm = row_parm
    wel$column_parm = column_parm
    wel$qfact_parm = qfact_parm
    if(!is.null(xyz_parm) && !is.null(wel$option) && (wel$option != "NOPRINT")) wel$xyz_parm = xyz_parm
    
  }
  
  # data set 5
  wel$itmp = itmp
  wel$np = np
  
  # data set 6
  if(any(wel$itmp > 0)){
    
    wel$layer_sp = layer_sp
    wel$row_sp = row_sp
    wel$column_sp = column_sp
    wel$q_sp = q_sp
    if(!is.null(xyz_sp) && !is.null(wel$option) && (wel$option != "NOPRINT")) wel$xyz_sp = xyz_sp
    
  }
  
  # data set 7
  if(any(wel$np > 0)){
    
    wel$pname = pname
    if(!is.null(wel$instances) && T %in% wel$instances) wel$iname = iname
    
  }
  
  
  class(wel) = c('wel', 'rmf_package')
  return(wel)
  
}