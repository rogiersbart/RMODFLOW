#' Create an \code{RMODFLOW} ghb object.
#' 
#' \code{rmf_create_ghb} creates an \code{RMODFLOW} ghb object
#' 
#' @param npghb number of ghb parameters; defaults to 1
#' @param mxl maximum number of general-head-boundary cells that will be defined using parameters; defaults to 10
#' @param mxactb maximum number of general-head-boundary cells in use during any stress period defined with and without parameters; defaults to 10
#' @param ighbcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param option optional character vector specifying the auxiliary variables and/or \code{NOPRINT}. Do not include the keywords \code{AUXILIARY} or \code{AUX}; defaults to NULL
#' @param parnam vector of length \code{npghb} specifying the parameter names; defaults to 'GHB'
#' @param parval vector of length \code{npghb} specifying the parameter values; defaults to 1
#' @param nlst vector of length \code{npghb} specifying the number of general-head-boundary cells that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to 10
#' @param instances logical vector of length \code{npghb} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{npghb} indicating the number instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{npghb} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param layer_parm list with \code{npghb} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the layer numbers of the model cells containing the general-head-boundaries specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 1 for all general-head-boundary cells for the parameter.
#' @param row_parm list with \code{npghb} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the row numbers of the model cells containing the general-head-boundaries specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 1:10 for all general-head-boundary cells for the parameter
#' @param column_parm list with \code{npghb} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the column numbers of the model cells containing the general-head-boundaries specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 1 for the parameter
#' @param bhead_parm list with \code{npghb} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the head on the boundary for parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to 5 for all general-head-boundary cells for the parameter
#' @param condfact_parm list with \code{npghb} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the conductance factor for parameter \code{i}. The conductance is the product of condfact and the parameter value. If not time-varying, set numinst dimension to 1. Defaults to 10 for all general-head-boundary cells for the parameter
#' @param xyz_parm optional list with \code{npghb} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nlst} specifying the auxiliary variables for parameter \code{i}. If not time-varying, set numinst to 1. Defaults to NULL
#' @param itmp vector of length \code{dis$nper} specifying non-parameter ghb data being read for each stress period; defaults to 0
#' @param np vector of length \code{dis$nper} specifying the number of ghb parameters in use for each stress period; defaults to 1
#' @param layer_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the layer numbers of the model cells containing the general-head-boundary cells; defaults to NULL
#' @param row_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the row numbers of the model cells containing the general-head-boundary cells; defaults to NULL
#' @param column_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the column numbers of the model cells containing the general-head-boundary cells; defaults to NULL
#' @param bhead_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the heads on the boundaries for model cells containing the general-head-boundary cells; defaults to NULL
#' @param cond_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the conductance of the model cells containing the general-head-boundary cells; defaults to NULL
#' @param xyz_sp optional list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{itmp} for stress period \code{i} specifying the auxiliary variables of the model cells containing the general-head-boundary cells; defaults to NULL
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameters being used; defaults to 'GHB'
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' 
#' @return \code{RMODFLOW} ghb object
#' @export
#' @seealso \code{\link{rmf_read_ghb}}, \code{\link{rmf_write_ghb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?ghb.htm}

rmf_create_ghb = function(npghb = 1,
                          mxl = 10,
                          mxactb = 10,
                          ighbcb = 0,
                          option = NULL,
                          parnam = 'GHB', 
                          parval = 1,
                          nlst = 10, 
                          instances = NULL,
                          numinst = NULL, 
                          instnam = NULL,
                          layer_parm = list(array(rep(1, 10), dim=c(1, 10))),
                          row_parm = list(array(1:10, dim=c(1, 10))),
                          column_parm = list(array(rep(1, 10), dim=c(1, 10))),
                          bhead_parm =  list(array(5, dim=c(1, 10))),
                          condfact_parm =  list(array(10, dim=c(1, 10))),
                          xyz_parm = NULL,
                          itmp = 0,
                          np = 1,
                          layer_sp = NULL,
                          row_sp = NULL,
                          column_sp = NULL,
                          bhead_sp = NULL,
                          cond_sp = NULL,
                          xyz_sp = NULL, 
                          pname = list('GHB'),
                          iname = NULL
){
  
  ghb = list()
  
  
  # data set 0
  # to provide comments, use ?comment on resulting ghb object
  
  # data set 1
  if (!is.null(npghb)){
    ghb$npghb = npghb
    ghb$mxl = mxl
  }
  
  
  # data set 2
  ghb$mxactb = mxactb
  ghb$ighbcb = ighbcb
  if(!is.null(option)) ghb$option = option
  
  if(!is.null(ghb$npghb) && ghb$npghb > 0){
    
    # data set 3
    ghb$parnam = parnam
    ghb$partyp = rep('GHB', ghb$npghb)
    ghb$parval = parval
    ghb$nlst = nlst
    if(!is.null(instances) && T %in% instances) ghb$instances = instances
    if(!is.null(ghb$instances)) ghb$numinst = numinst
    
    # data set 4a
    if(!is.null(ghb$instances) && T %in% ghb$instances) ghb$instnam = instnam
    
    # data set 4b
    ghb$layer_parm = layer_parm
    ghb$row_parm = row_parm
    ghb$column_parm = column_parm
    ghb$bhead_parm = bhead_parm
    ghb$condfact_parm = condfact_parm
    if(!is.null(xyz_parm) && !is.null(ghb$option) && (ghb$option != "NOPRINT")) ghb$xyz_parm = xyz_parm
    
  }
  
  # data set 5
  ghb$itmp = itmp
  ghb$np = np
  
  # data set 6
  if(any(ghb$itmp > 0)){
    
    ghb$layer_sp = layer_sp
    ghb$row_sp = row_sp
    ghb$column_sp = column_sp
    ghb$bhead_sp = bhead_sp
    ghb$cond_sp = cond_sp
    if(!is.null(xyz_sp) && !is.null(ghb$option) && (ghb$option != "NOPRINT")) ghb$xyz_sp = xyz_sp
    
  }
  
  # data set 7
  if(any(ghb$np > 0)){
    
    ghb$pname = pname
    if(!is.null(ghb$instances) && T %in% ghb$instances) ghb$iname = iname
    
  }
  
  
  class(ghb) = c('ghb', 'rmf_package')
  return(ghb)
  
}