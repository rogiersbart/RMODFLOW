#' Create an \code{RMODFLOW} chd object.
#' 
#' \code{rmf_create_chd} creates an \code{RMODFLOW} chd object
#' 
#' @param npchd number of chd parameters; defaults to NULL
#' @param mxl maximum number of constant-head boundaries that will be defined using parameters; defaults to NULL
#' @param mxactc maximum number of constant-head boundaries in use during any stress period defined with and without parameters; defaults to 10
#' @param option optional character vector specifying the auxiliary variables and/or \code{NOPRINT}. Do not include the keywords \code{AUXILIARY} or \code{AUX}; defaults to NULL
#' @param parnam vector of length \code{npchd} specifying the parameter names; defaults to NULL
#' @param parval vector of length \code{npchd} specifying the parameter values; defaults to NULL
#' @param nlst vector of length \code{npchd} specifying the number of constant-head cells that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to NULL
#' @param instances logical vector of length \code{npchd} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{npchd} indicating the number instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{npchd} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param layer_parm list with \code{npchd} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the layer numbers of the model cells containing the constant-head boundaries specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param row_parm list with \code{npchd} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the row numbers of the model cells containing the constant-head boundaries specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param column_parm list with \code{npchd} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying the column numbers of the model cells containing the constant-head boundaries specified by parameter \code{i}. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param shdfact_parm list with \code{npchd} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying a factor used to calculate the head at the boundary at the start of the stress period for parameter \code{i}. The starting head at the boundary is the product of shdfact and the parameter value. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param edhfact_parm list with \code{npchd} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nlst} specifying a factor used to calculate the head at the boundary at the end of the stress period for parameter \code{i}. The ending head at the boundary is the product of ehdfact and the parameter value. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param xyz_parm optional list with \code{npchd} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nlst} specifying the auxiliary variables for parameter \code{i}. If not time-varying, set numinst to 1. Defaults to NULL
#' @param itmp vector of length \code{dis$nper} specifying non-parameter constant-head data being read for each stress period; defaults to 10
#' @param np vector of length \code{dis$nper} specifying the number of constant-head parameters in use for each stress period; defaults to 0
#' @param layer_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the layer numbers of the model cells containing the constant-head boundaries; defaults to 1 for all constant-head boundaries
#' @param row_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the row numbers of the model cells containing the constant-head boundaries; defaults to 1:10
#' @param column_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the column numbers of the model cells containing the constant-head boundaries; defaults to 1 for all constant-head boundaries
#' @param shead_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the starting heads at the constant-head cells; defaults to 0 for all constant-head boundaries
#' @param ehead_sp list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{itmp} for stress period \code{i} specifying the ending heads at the constant-head cells; defaults to -1 for all constant-head boundaries
#' @param xyz_sp optional list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{itmp} for stress period \code{i} specifying the auxiliary variables of the model cells containing the constant-head boundaries; defaults to NULL
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameters being used; defaults to NULL
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{np} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' 
#' @return \code{RMODFLOW} chd object
#' @export
#' @seealso \code{\link{rmf_read_chd}}, \code{\link{rmf_write_chd}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?chd.htm}

rmf_create_chd = function(npchd = NULL,
                          mxl = NULL,
                          mxactc = 10,
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
                          shdfact_parm =  NULL,
                          ehdfact_parm =  NULL,
                          xyz_parm = NULL,
                          itmp = 10,
                          np = 0,
                          layer_sp = list(rep(1, 10)),
                          row_sp = list(1:10),
                          column_sp = list(rep(1, 10)),
                          shead_sp = list(rep(0, 10)),
                          ehead_sp = list(rep(-1, 10)),
                          xyz_sp = NULL, 
                          pname = NULL,
                          iname = NULL
){
  
  chd = list()
  
  
  # data set 0
  # to provide comments, use ?comment on resulting chd object
  
  # data set 1
  if (!is.null(npchd)){
    chd$npchd = npchd
    chd$mxl = mxl
  }
  
  
  # data set 2
  chd$mxactc = mxactc
  if(!is.null(option)) chd$option = option
  
  if(!is.null(chd$npchd) && chd$npchd > 0){
    
    # data set 3
    chd$parnam = parnam
    chd$partyp = rep('CHD', chd$npchd)
    chd$parval = parval
    chd$nlst = nlst
    if(!is.null(instances) && T %in% instances) chd$instances = instances
    if(!is.null(chd$instances)) chd$numinst = numinst
    
    # data set 4a
    if(!is.null(chd$instances) && T %in% chd$instances) chd$instnam = instnam
    
    # data set 4b
    chd$layer_parm = layer_parm
    chd$row_parm = row_parm
    chd$column_parm = column_parm
    chd$shdfact_parm = shdfact_parm
    chd$ehdfact_parm = ehdfact_parm
    if(!is.null(xyz_parm) && !is.null(chd$option) && (chd$option != "NOPRINT")) chd$xyz_parm = xyz_parm
    
  }
  
  # data set 5
  chd$itmp = itmp
  chd$np = np
  
  # data set 6
  if(any(chd$itmp > 0)){
    
    chd$layer_sp = layer_sp
    chd$row_sp = row_sp
    chd$column_sp = column_sp
    chd$shead_sp = shead_sp
    chd$ehead_sp = ehead_sp
    if(!is.null(xyz_sp) && !is.null(chd$option) && (chd$option != "NOPRINT")) chd$xyz_sp = xyz_sp
    
  }
  
  # data set 7
  if(any(chd$np > 0)){
    
    chd$pname = pname
    if(!is.null(chd$instances) && T %in% chd$instances) chd$iname = iname
    
  }
  
  
  class(chd) = c('chd', 'rmf_package')
  return(chd)
  
}