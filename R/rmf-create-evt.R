#' Create an \code{RMODFLOW} evt object.
#' 
#' \code{rmf_create_evt} creates an \code{RMODFLOW} evt object
#' ' 
#' @param npevt number of evt parameters; defaults to NULL
#' @param nevtop evapotranspiration option code; defaults to 1
#' @param ievtcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param parnam vector of length \code{npevt} specifying the parameter names; defaults to NULL
#' @param parval vector of length \code{npevt} specifying the parameter values; defaults to NULL
#' @param nclu vector of length \code{npevt} specifying the number of clusters that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to NULL
#' @param instances logical vector of length \code{npevt} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{npevt} indicating the number of instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{npevt} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param mltarr list with \code{npevt} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} specifying the multiplier array name of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param zonarr list with \code{npevt} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nclu} specifying the zone array name of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param iz list with \code{npevt} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} specifying the zone numbers of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param insurf numeric vector of length \code{dis$nper} specifying the \code{surf} read flag; defaults to 1
#' @param inevtr numeric vector of length \code{dis$nper} specifying the \code{evtr} read flag which depends on whether or not parameters are specified; defaults to 1
#' @param inexdp numeric vector of length \code{dis$nper} specifying the \code{exdp} read flag; defaults to 1
#' @param inievt numeric vector of length \code{dis$nper} specifying the \code{ievt} read flag; defaults to NULL
#' @param surf numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the elevation of the ET surface; defaults to the 0 for all cells
#' @param evtr numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the maximum ET flux; defaults to 4e-9 for all cells
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{inevtr} for stress period \code{i} specifying the names of the parameters being used; defaults to NULL
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{inevtr} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' @param ievtpf optional list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{inevt} for stress period \code{i} specifying the format code for printing \code{evtr} after it has been defined by parameters, defaults to NULL 
#' @param exdp numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the ET extinction depths as distances from \code{surf}; defaults to 2 for all cells
#' @param ievt numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the layer numbers defining in which layer ET is removed; defaults to NULL
#' 
#' @return \code{RMODFLOW} evt object
#' @export
#' @seealso \code{\link{rmf_read_evt}}, \code{\link{rmf_write_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}


rmf_create_evt = function(npevt = NULL,
                          nevtop = 1,
                          ievtcb = 0,
                          parnam = NULL, 
                          parval = NULL, 
                          nclu = NULL, 
                          instances = NULL,
                          numinst = NULL,
                          instnam = NULL,
                          mltarr = NULL, 
                          zonarr = NULL,
                          iz = NULL,
                          insurf = 1,
                          inevtr = 1,
                          inexdp = 1,
                          inievt = NULL,
                          pname = NULL, 
                          iname = NULL,
                          ievtpf = NULL,
                          surf = array(0, dim=c(10, 10, 1)), 
                          evtr = array(4e-9, dim=c(10, 10, 1)), 
                          exdp = array(2, dim=c(10, 10, 1)),
                          ievt = NULL
){
  
  evt = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting evt object
  
  # data set 1
  if(!is.null(npevt)) evt$npevt = npevt
  
  # data set 2
  evt$nevtop = nevtop
  evt$ievtcb = ievtcb
  
  
  if(!is.null(evt$npevt) && evt$npevt > 0){
    
    # data set 3
    evt$parnam = parnam
    evt$partyp = rep('EVT', evt$npevt)
    evt$parval = parval
    evt$nclu = nclu
    if(!is.null(instances) && T %in% instances) evt$instances = instances
    if(!is.null(evt$instances)) evt$numinst = numinst      
    
    
    # data set 4a
    if(!is.null(evt$instances) && T %in% evt$instances) evt$instnam = instnam
    
    # data set 4b
    evt$mltarr = mltarr
    evt$zonarr = zonarr
    evt$iz = iz
    
  }
  
  
  # data set 5
  evt$insurf = insurf
  evt$inevtr = inevtr
  evt$inexdp = inexdp
  if(evt$nevtop==2) evt$inievt = inievt
  
  # data set 6
  if(any(evt$insurf >= 0)) evt$surf = surf
  
  # data set 7
  if ((is.null(evt$npevt) || (!is.null(evt$npevt) && evt$npevt == 0)) && any(evt$inevtr >= 0)) evt$evtr = evtr
  
  # data set 8
  if ((!is.null(evt$npevt) && evt$npevt > 0) && any(evt$inevtr > 0)){
    evt$pname = pname
    if (!is.null(evt$instances) && T %in% evt$instances) evt$iname = iname
    if (!is.null(ievtpf) && (is.null(evt$instances) || (!is.null(evt$instances) && !(T %in% evt$instances))) ) evt$ievtpf = ievtpf
  } 
  
  # data set 9
  if(any(evt$inexdp >= 0)) evt$exdp = exdp
  
  # data set 10
  if(evt$nevtop == 2 && any(evt$inievt >= 0)) evt$ievt = ievt
  
  class(evt) = c('evt', 'rmf_package')
  return(evt)
  
}
