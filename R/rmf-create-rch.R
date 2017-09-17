#' Create an \code{RMODFLOW} rch object.
#' 
#' \code{rmf_create_rch} creates an \code{RMODFLOW} rch object
#' 
#' @param nprch number of rch parameters; defaults to NULL
#' @param nrchop recharge option code; defaults to 1
#' @param irchcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param parnam vector of length \code{nprch} specifying the parameter names; defaults to NULL
#' @param parval vector of length \code{nprch} specifying the parameter values; defaults to NULL
#' @param nclu vector of length \code{nprch} specifying the number of clusters that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to NULL
#' @param instances logical vector of length \code{nprch} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{nprch} indicating the number of instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{nprch} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param mltarr list with \code{nprch} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} specifying the multiplier array name of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param zonarr list with \code{nprch} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nclu} specifying the zone array name of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param iz list with \code{nprch} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} specifying the zone numbers of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param inrech numeric vector of length \code{dis$nper} specifying the \code{rech} read flag which depends on whether or not parameters are specified; defaults to 1
#' @param inirch numeric vector of length \code{dis$nper} specifying the \code{irch} read flag which depends on whether or not parameters are specified; defaults to NULL
#' @param rech numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the recharge fluxes; defaults to 1e-8 for all cells in the top layer
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{inrech} for stress period \code{i} specifying the names of the parameters being used; defaults to NULL
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{inrech} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' @param irchpf optional list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{inrech} for stress period \code{i} specifying the format code for printing \code{rech} after it has been defined by parameters, defaults to NULL 
#' @param irch numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the layer numbers defining in which layer recharge is applied; defaults to NULL
#' 
#' @return \code{RMODFLOW} rch object
#' @export
#' @seealso \code{\link{rmf_read_rch}}, \code{\link{rmf_write_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}


rmf_create_rch = function(nprch = NULL,
                      nrchop = 1,
                      irchcb = 0,
                      parnam = NULL, 
                      parval = NULL, 
                      nclu = NULL, 
                      instances = NULL,
                      numinst = NULL,
                      instnam = NULL,
                      mltarr = NULL, 
                      zonarr = NULL,
                      iz = NULL,
                      inrech = 1,
                      inirch = NULL,
                      rech = array(1e-8, dim=c(10, 10, 1)), 
                      pname = NULL, 
                      iname = NULL,
                      irchpf = NULL,
                      irch = NULL
                      ){
  
  rch = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting rch object
  
  # data set 1
  if(!is.null(nprch)) rch$nprch = nprch

  # data set 2
  rch$nrchop = nrchop
  rch$irchcb = irchcb
  
  
  if(!is.null(rch$nprch) && rch$nprch > 0){
    
    # data set 3
    rch$parnam = parnam
    rch$partyp = rep('RCH', rch$nprch)
    rch$parval = parval
    rch$nclu = nclu
    if(!is.null(instances) && T %in% instances) rch$instances = instances
    if(!is.null(rch$instances)) rch$numinst = numinst      

    
    # data set 4a
    if(!is.null(rch$instances) && T %in% rch$instances) rch$instnam = instnam
    
    # data set 4b
    rch$mltarr = mltarr
    rch$zonarr = zonarr
    rch$iz = iz
    
  }
  
  
  # data set 5
  rch$inrech = inrech
  if(rch$nrchop==2) rch$inirch = inirch
  
  # data set 6
  if ((is.null(rch$nprch) || (!is.null(rch$nprch) && rch$nprch == 0)) && any(rch$inrech >= 0)) rch$rech = rech
  
  # data set 7
  if ((!is.null(rch$nprch) && rch$nprch > 0) && any(rch$inrech > 0)){
    rch$pname = pname
    if (!is.null(rch$instances) && T %in% rch$instances) rch$iname = iname
    if (!is.null(irchpf) && (is.null(rch$instances) || (!is.null(rch$instances) && !(T %in% rch$instances))) ) rch$irchpf = irchpf
  } 
  
  # data set 8
  if(rch$nrchop == 2 && any(rch$inirch >= 0)) rch$irch = irch
  
  class(rch) = c('rch', 'rmf_package')
  return(rch)
  
}
