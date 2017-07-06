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
#' @param parnam vector of parameter names; names should not be more than 10 characters, are not case sensitive, and should be unique
#' @param partyp vector of parameter types; the lpf parameter types are HK, HANI, VK, VANI, SS, SY, or VKCB
#' @param parval vector of parameter values
#' @param nclu vector with the number of clusters required for each parameter
#' @param mltarr matrix of multiplier array names, with dis$nlay rows and lpf$nplpf columns; cells with non-occurring layer-parameter combinations should be NA
#' @param zonarr matrix of zone array names, with dis$nlay rows and lpf$nplpf columns; cells with non-occurring layer-parameter combinations should be NA
#' @param iz matrix of zone number combinations separated by spaces, with dis$nlay rows and lpf$nplpf columns; cells with non-occurring layer-parameter combinations should be NA; if zonarr is "ALL", iz should be ""
#' @param hk 3d array with hydraulic conductivity along rows; defaults to 1
#' @param hani 3d array with the ratio of hydraulic conductivity along columns to that along rows; defaults to 1
#' @param vka 3d array with vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to hk
#' @param ss 3d array with specific storage; only required when there are transient stress periods; defaults to 1E-5
#' @param sy 3d array with specific yield; only required when there are transient stress periods; defaults to 0.15
#' @param vkcb 3d array with vertical hydraulic conductivity of quasi-three-dimensional confining beds; defaults to 0
#' @param wetdry 3d array with a wetting threshold and flag indicating which neighboring cells can cause a cell to become wet; defaults to -0.01
#' @return Object of class lpf
#' @export
#' @seealso \code{\link{read_lpf}}, \code{\link{write_lpf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm}
rmf_create_lpf <- function(dis = create_dis(),
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
                         parnam = NULL,
                         partyp = NULL,
                         parval = NULL,
                         nclu = NULL,
                         mltarr = NULL,
                         zonarr = NULL,
                         iz = NULL,
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
    lpf$parnam <- parnam
    lpf$partyp <- partyp
    lpf$parval <- parval
    lpf$nclu <- nclu
    lpf$mltarr <- mltarr
    lpf$zonarr <- zonarr
    lpf$iz <- iz

  # data set 10-16
    lpf$hk <- rmf_create_array(hk)
    lpf$hani <- rmf_create_array(hani)
    lpf$vka <- rmf_create_array(vka)
    lpf$ss <- rmf_create_array(ss)
    lpf$sy <- rmf_create_array(sy)
    lpf$vkcb <- rmf_create_array(vkcb)
    lpf$wetdry <- rmf_create_array(wetdry)
    
  class(lpf) <- c('lpf','rmf_package')
  return(lpf)
}

#' @describeIn rmf_create_lpf Deprecated function name
create_lpf <- function(...) {
  .Deprecated(new = "rmf_create_lpf", old = "create_lpf")
  rmf_create_lpf(...)
}
