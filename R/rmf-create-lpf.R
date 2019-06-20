#' Create an \code{RMODFLOW} lpf object
#' 
#' \code{rmf_create_lpf} creates an \code{RMODFLOW} lpf object.
#' 
#' @param dis RMODFLOW dis object
#' @param ilpfcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
#' @param nplpf number of lpf parameters; defaults to 0
#' @param storagecoefficient logical; should STORAGECOEFFICIENT keyword be included?; defaults to FALSE
#' @param constantcv logical; should CONSTANTCV keyword be included?; defaults to FALSE
#' @param thickstrt logical; should THICKSTRT keyword be included?; defaults to FALSE
#' @param nocvcorrection logical; should NOCVCORRECTION keyword be included?; defaults to FALSE
#' @param novfc logical; should NOVFC keyword be included?; defaults to FALSE
#' @param noparcheck logical; should NOPARCHECK keyword be included?; defaults to FALSE
#' @param laytyp vector of flags for each layer, specifying layer type; defaults to all confined (0) except the first layer (1)
#' @param layavg vector of flags for each layer, specifying interblock transmissivity calculation method; defaults to 0 for each layer
#' @param chani vector of flags or horizontal anisotropies for each layer; defaults to 1 for each layer
#' @param layvka vector of flags for each layer, indicating whether vka is the vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to 0 for each layer
#' @param laywet vector of flags for each layer, indicating if wetting is active; defaults to 0 for each layer
#' @param wetfct is a factor that is included in the calculation of the head that is initially established at a cell when it is converted from dry to wet; defaults to 0.1
#' @param iwetit is the iteration interval for attempting to wet cells; defaults to 1
#' @param ihdwet is a flag that determines which equation is used to define the initial head at cells that become wet; defaults to 0
#' @param parameters list of \code{rmf_parameter} as created by \code{\link{rmf_create_flow_parameter}}. See details; defaults to NULL
#' @param hk 3d array with hydraulic conductivity along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param hani 3d array with the ratio of hydraulic conductivity along columns to that along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param vka 3d array with vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to hk. If not read for a specific layer, set all values in that layer to NA.
#' @param ss 3d array with specific storage; only required when there are transient stress periods; defaults to 1E-5. If not read for a specific layer, set all values in that layer to NA.
#' @param sy 3d array with specific yield; only required when there are transient stress periods; defaults to 0.15. If not read for a specific layer, set all values in that layer to NA.
#' @param vkcb 3d array with vertical hydraulic conductivity of quasi-three-dimensional confining beds; defaults to 0. If not read for a specific layer, set all values in that layer to NA.
#' @param wetdry 3d array with a wetting threshold and flag indicating which neighboring cells can cause a cell to become wet; defaults to -0.01. If not read for a specific layer, set all values in that layer to NA.
#' @details Flow variables are any of \code{HK, HANI, VK, VANI, SS, SY and VKCB}. A single variable can be specified either through the use of parameters are by using direct array input.
#'          When a flow variable for a specific layer is specified using parameters, all flow variables of the type must be specified by parameters. E.g. if a flow parameter defines HK for layer 1, HK must be defined for all layers using flow parameters instead of direct array input.
#' @return Object of class lpf
#' @export
#' @seealso \code{\link{rmf_create_flow_parameter}}, \code{\link{rmf_read_lpf}}, \code{\link{rmf_write_lpf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm}
rmf_create_lpf <- function(dis = rmf_create_dis(),
                           ilpfcb = 0,
                           hdry = -888,
                           nplpf = 0,
                           storagecoefficient = FALSE,
                           constantcv = FALSE,
                           thickstrt = FALSE,
                           nocvcorrection = FALSE,
                           novfc = FALSE,
                           noparcheck = FALSE,
                           laytyp = ifelse(dis$nlay == 1, list(1), list(c(1,rep(0, dis$nlay - 1))))[[1]],
                           layavg = laytyp * 0,
                           chani = rep(1, dis$nlay),
                           layvka = rep(0, dis$nlay),
                           laywet = rep(0, dis$nlay),
                           wetfct = 0.1,
                           iwetit = 1,
                           ihdwet = 0,
                           parameters = NULL,
                           hk = rmf_create_array(0.0001, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           hani = rmf_create_array(1, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           vka = hk,
                           ss = rmf_create_array(1E-5, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           sy = rmf_create_array(0.15, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           vkcb = rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           wetdry = rmf_create_array(-0.01, dim = c(dis$nrow, dis$ncol, dis$nlay))) {
  
  lpf <- NULL
  
  # data set 0
  # to provide comments, use ?comment on the resulting lpf object
  
  # data set 1
  lpf$ilpfcb <- ilpfcb
  lpf$hdry <- hdry
  lpf$nplpf <- nplpf
  lpf$storagecoefficient <- storagecoefficient
  lpf$constantcv <- constantcv
  lpf$thickstrt <- thickstrt
  lpf$nocvcorrection <- nocvcorrection
  lpf$novfc <- novfc
  lpf$noparcheck <- noparcheck
  
  # data set 2
  lpf$laytyp <- laytyp
  
  # data set 3
  lpf$layavg <- layavg
  
  # data set 4
  lpf$chani <- chani
  
  # data set 5
  lpf$layvka <- layvka
  
  # data set 6
  lpf$laywet <- laywet
  
  # data set 7
  if(!as.logical(prod(lpf$laywet==0))) {
    lpf$wetfct <- wetfct
    lpf$iwetit <- iwetit
    lpf$ihdwet <- ihdwet
  }
  
  # data set 8-9
  types <- NULL
  if(!is.null(parameters)) {

    # error check
    if(any(vapply(parameters, function(i) is.null(attr(i, 'type')) || is.null(attr(i, 'layer')) || is.null(attr(i, 'name')) || is.null(attr(i, 'value')), TRUE))) {
      stop('Please make sure all parameters are flow parameters that have a name, value, type and layer attribute')
    }
    types <- toupper(unique(vapply(parameters, function(i) attr(i, 'type'), 'HK')))
    
  }
  
  # data set 10-16
  if(!("HK" %in% types)) lpf$hk <- rmf_create_array(hk, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("HANI" %in% types) && any(lpf$chani <= 0)) lpf$hani <- rmf_create_array(hani, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("VK" %in% types | "VANI" %in% types)) lpf$vka <- rmf_create_array(vka, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("SS" %in% types) && 'TR' %in% dis$sstr) lpf$ss <- rmf_create_array(ss, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("SY" %in% types) && 'TR' %in% dis$sstr && any(lpf$laytyp != 0)) lpf$sy <- rmf_create_array(sy, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("VKCB" %in% types) && any(dis$laycbd != 0)) lpf$vkcb <- rmf_create_array(vkcb, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(any(lpf$laywet != 0) && any(lpf$laytyp != 0)) lpf$wetdry <- rmf_create_array(wetdry, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  class(lpf) <- c('lpf','rmf_package')
  return(lpf)
}

#' @describeIn rmf_create_lpf Deprecated function name
#' @export
create_lpf <- function(...) {
  .Deprecated(new = "rmf_create_lpf", old = "create_lpf")
  rmf_create_lpf(...)
}
