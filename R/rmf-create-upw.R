#' Create an \code{RMODFLOW} upw object
#' 
#' \code{rmf_create_upw} creates an \code{RMODFLOW} upw object.
#' 
#' @param dis RMODFLOW dis object
#' @param iupwcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
#' @param npupw number of upw parameters; defaults to 0
#' @param iphdry logical; indicating if head will be set to hdry when it's less than 1E-4 above the cell bottom; defaults to TRUE
#' @param laytyp vector of flags for each layer, specifying layer type; defaults to all confined (0) except the first layer (1)
#' @param layavg vector of flags for each layer, specifying interblock transmissivity calculation method; defaults to 0 for each layer
#' @param chani vector of flags or horizontal anisotropies for each layer; defaults to 1 for each layer
#' @param layvka vector of flags for each layer, indicating whether vka is the vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to 0 for each layer
#' @param parnam vector of parameter names; names should not be more than 10 characters, are not case sensitive, and should be unique
#' @param partyp vector of parameter types; the upw parameter types are HK, HANI, VK, VANI, SS, SY, or VKCB
#' @param parval vector of parameter values
#' @param nclu vector with the number of clusters required for each parameter
#' @param mltarr matrix of multiplier array names, with dis$nlay rows and upw$npupw columns; cells with non-occurring layer-parameter combinations should be NA
#' @param zonarr matrix of zone array names, with dis$nlay rows and upw$npupw columns; cells with non-occurring layer-parameter combinations should be NA
#' @param iz character matrix of zone number combinations separated by spaces, with dis$nlay rows and upw$npupw columns; cells with non-occurring layer-parameter combinations should be NA; if zonarr is "ALL", iz should be ""
#' @param hk 3d array with hydraulic conductivity along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param hani 3d array with the ratio of hydraulic conductivity along columns to that along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param vka 3d array with vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to hk. If not read for a specific layer, set all values in that layer to NA.
#' @param ss 3d array with specific storage; only required when there are transient stress periods; defaults to 1E-5. If not read for a specific layer, set all values in that layer to NA.
#' @param sy 3d array with specific yield; only required when there are transient stress periods; defaults to 0.15. If not read for a specific layer, set all values in that layer to NA.
#' @param vkcb 3d array with vertical hydraulic conductivity of quasi-three-dimensional confining beds; defaults to 0. If not read for a specific layer, set all values in that layer to NA.
#' @return Object of class upw
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @note upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
#' @export
#' @seealso \code{\link{rmf_read_upw}}, \code{\link{rmf_write_upw}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_create_upw <- function(dis = rmf_create_dis(),
                           iupwcb = 0,
                           hdry = -888,
                           npupw = 0,
                           iphdry = TRUE,
                           laytyp = ifelse(dis$nlay == 1, list(1), list(c(1,rep(0, dis$nlay - 1))))[[1]],
                           layavg = laytyp * 0,
                           chani = rep(1, dis$nlay),
                           layvka = rep(0, dis$nlay),
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
                           vkcb = rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))) {

  upw <- NULL
  
  # data set 0
  # to provide comments, use ?comment on the resulting upw object
  
  # data set 1
  upw$iupwcb <- iupwcb
  upw$hdry <- hdry
  upw$npupw <- npupw
  upw$iphdry <- iphdry
  
  # data set 2
  upw$laytyp <- laytyp
  
  # data set 3
  upw$layavg <- layavg
  
  # data set 4
  upw$chani <- chani
  
  # data set 5
  upw$layvka <- layvka
  
  # data set 6
  upw$laywet <- rep(0, dis$nlay)
  
  # data set 7-8
  upw$parnam <- parnam
  upw$partyp <- partyp
  upw$parval <- parval
  upw$nclu <- nclu
  upw$mltarr <- mltarr
  upw$zonarr <- zonarr
  upw$iz <- iz
  
  # data set 9-14
  if(!("HK" %in% upw$partyp)) upw$hk <- rmf_create_array(hk, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("HANI" %in% upw$partyp) && any(upw$chani <= 0)) upw$hani <- rmf_create_array(hani, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("VK" %in% upw$partyp | "VANI" %in% upw$partyp)) upw$vka <- rmf_create_array(vka, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("SS" %in% upw$partyp) && 'TR' %in% dis$sstr) upw$ss <- rmf_create_array(ss, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("SY" %in% upw$partyp) && 'TR' %in% dis$sstr && any(upw$laytyp != 0)) upw$sy <- rmf_create_array(sy, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(!("VKCB" %in% upw$partyp) && any(dis$laycbd != 0)) upw$vkcb <- rmf_create_array(vkcb, dim = c(dis$nrow, dis$ncol, dis$nlay))

  class(upw) <- c('upw','rmf_package')
  return(upw)
}


