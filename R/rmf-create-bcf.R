#' Create an \code{RMODFLOW} bcf object
#' 
#' \code{rmf_create_bcf} creates an \code{RMODFLOW} bcf object
#'
#' @param ibcfcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
#' @param iwdflg wetting capability flag; defaults to 0
#' @param wetfct factor included in the calculation of the initial head when a cell is converted from dry to wet; defaults to 1
#' @param iwetit iteration interval for attempting to wet cells; defaults to 1
#' @param ihdwet flag determining which equation is used to define initial heads at cells that are converted from dry to wet; defaults to 0
#' @param layavg numeric vector of length \code{dis$nlay} determining which method of calculating interblock transmissivity to use for each layer; defaults to 0 for 3 layers
#' @param laycon numeric vector of length \code{dis$nlay} determining which layer type (LAYCON) to use for each layer; defaults to 0 for 3 layer
#' @param trpy numeric vector of length \code{dis$nlay} determining the horizontal anisotropy of each layer; defaults to 1 for 3 layers
#' @param sf1 numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the primary storage coefficient for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to NULL
#' @param tran numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the transmissivity along rows for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to 0.001 for each cell
#' @param hy numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the hydraulic conductivity along rows for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to NULL
#' @param vcont numeric 3D array of dimensions \code{dis$nrow x dis$ncol x (dis$nlay-1)} specifying the vertical hydraulic conductivity divided by the thickness between the node in the cell and the node in the cell below. If not read for a specific layer, set all values in that layer to NA; defaults to 1e-5 for each cell
#' @param sf2 numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the secondary storage coefficient for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to NULL    
#' @param wetdry numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the flag and wetting threshold for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to NULL
#' 
#' @return \code{RMODFLOW} bcf object
#' @export
#' @seealso \code{\link{rmf_read_bcf}}, \code{\link{rmf_write_bcf}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}

rmf_create_bcf = function(dis = rmf_create_dis(),
                          ibcfcb = 0,
                          hdry = -888,
                          iwdflg = 0,
                          wetfct = 1,
                          iwetit = 1,
                          ihdwet = 0,
                          layavg = rep(0, dis$nlay),
                          laycon = rep(0, dis$nlay),
                          trpy = rep(1, dis$nlay),
                          sf1 = NULL,
                          tran = array(0.001, dim=c(dis$nrow, dis$ncol, dis$nlay)),
                          hy = NULL,
                          vcont = array(1e-5, dim=c(dis$nrow, dis$ncol, dis$nlay)),
                          sf2 = NULL,
                          wetdry = NULL
                          ){
  
  bcf = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting bcf object
  
  # data set 1
  bcf$ibcfcb = ibcfcb
  bcf$hdry = hdry
  bcf$iwdflg = iwdflg
  bcf$wetfct = wetfct
  bcf$iwetit = iwetit
  bcf$ihdwet = ihdwet
  
  # data set 2
  bcf$layavg = layavg
  bcf$laycon = laycon
  
  # data set 3
  bcf$trpy = trpy
  
  # data set 4
  if(!is.null(sf1)) bcf$sf1 = rmf_create_array(sf1, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 5
  if(!is.null(tran)) bcf$tran = rmf_create_array(tran, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 6
  if(!is.null(hy)) bcf$hy = rmf_create_array(hy, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 7
  bcf$vcont = rmf_create_array(vcont, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 8
  if(!is.null(sf2)) bcf$sf2 = rmf_create_array(sf2, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 9
  if(!is.null(wetdry)) bcf$wetdry = rmf_create_array(wetdry, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  class(bcf) = c('bcf', 'rmf_package')
  return(bcf)
}