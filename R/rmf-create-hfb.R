#' Create an \code{RMODFLOW} hfb object.
#' 
#' \code{rmf_create_hfb} creates an \code{RMODFLOW} hfb object
#' 
#' @param nphfb number of hfb parameters; defaults to 0
#' @param mxfb maximum number of hfb barriers that will be defined using parmeters; defaults to 0
#' @param nhfbnp number of hfb barriers not defined by parameters; defaults to 1
#' @param option optional character vector specifying \code{NOPRINT}; defaults to NULL
#' @param parnam vector of length \code{nphfb} specifying the parameter names; defaults to NULL
#' @param parval vector of length \code{nphfb} specifying the parameter values; defaults to NULL
#' @param nlst vector of length \code{nphfb} specifying the number of horizontal-flow barrier cells included in the parameter; defaults to NULL
#' @param layer_parm list with \code{nphfb} elements where each element \code{i} is a numeric vector of length \code{nlst} for parameter \code{i} specifying the layer numbers of the model cells containing the horizontal-flow barriers for parameter \code{i}; defaults to NULL
#' @param irow1_parm list with \code{nphfb} elements where each element \code{i} is a numeric vector of length \code{nlst} for parameter \code{i} specifying the row numbers of the model cells on one side of the cells containing the horizontal-flow barriers for parameter \code{i}; defaults to NULL
#' @param icol1_parm list with \code{nphfb} elements where each element \code{i} is a numeric vector of length \code{nlst} for parameter \code{i} specifying the column numbers of the model cells on one side of the cells containing the horizontal-flow barriers for parameter \code{i}; defaults to NULL
#' @param irow2_parm list with \code{nphfb} elements where each element \code{i} is a numeric vector of length \code{nlst} for parameter \code{i} specifying the row numbers of the model cells on the other side of the cells containing the horizontal-flow barriers for parameter \code{i}; defaults to NULL
#' @param icol2_parm list with \code{nphfb} elements where each element \code{i} is a numeric vector of length \code{nlst} for parameter \code{i} specifying the column numbers of the model cells on the other side of the cells containing the horizontal-flow barriers for parameter \code{i}; defaults to NULL
#' @param factor_parm list with \code{nphfb} elements where each element \code{i} is a numeric vector of length \code{nlst} for parameter \code{i} specifying the factor used to calculate the hydraulic characteristic from the parameter value of the model cells containing the horizontal-flow barriers for parameter \code{i}. The hydraulic characteristic is the product of factor and the parameter value; defaults to NULL
#' @param layer_noparm numeric vector of length \code{nhfbnp} specifying the layer numbers of the model cells containing the horizontal-flow barriers; defaults to 2 for all horizontal-flow barriers
#' @param irow1_noparm numeric vector of length \code{nhfbnp} specifying the row numbers of the model cells on one side of the cells containing the horizontal-flow barriers; defaults to 3 for all horizontal-flow barriers 
#' @param icol1_noparm numeric vector of length \code{nhfbnp} specifying the column numbers of the model cells on one side of the cells containing the horizontal-flow barriers; defaults to 7 for all horizontal-flow barriers
#' @param irow2_noparm numeric vector of length \code{nhfbnp} specifying the row numbers of the model cells on the other side of the cells containing the horizontal-flow barriers; defaults to 4 for all horizontal-flow barriers
#' @param icol2_noparm numeric vector of length \code{nhfbnp} specifying the column numbers of the model cells on the other side of the cells containing the horizontal-flow barriers; defaults to 7 for all horizontal-flow barriers
#' @param hydchr_noparm numeric vector of length \code{nhfbnp} specifying the hydraulic characteristic of the model cells containing the horizontal-flow barriers. The hydraulic characteristic is the product of factor and the parameter value; defaults to -0.01
#' @param nacthfb number of active hfb parameters; defaults to 0
#' @param pname character vector of length \code{nacthfb} specifying the names of the active hfb parameters; defaults to NULL
#'
#' @return \code{RMODFLOW} hfb object
#' @export
#' @seealso \code{\link{rmf_read_hfb}}, \code{\link{rmf_write_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}


rmf_create_hfb = function(nphfb = 0,
                          mxfb = 0,
                          nhfbnp = 1,
                          option = NULL,
                          parnam = NULL,
                          parval = NULL,
                          nlst = NULL,
                          layer_parm = NULL,
                          irow1_parm = NULL,
                          icol1_parm = NULL,
                          irow2_parm= NULL,
                          icol2_parm = NULL,
                          factor_parm = NULL,
                          layer_noparm = 2,
                          irow1_noparm = 3,
                          icol1_noparm = 7,
                          irow2_noparm = 4,
                          icol2_noparm = 7,
                          hydchr_noparm = -1.5,
                          nacthfb = 0,
                          pname = NULL
){
  
  hfb = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting hfb object
  
  # data set 1
  hfb$nphfb = nphfb
  hfb$mxfb = mxfb
  hfb$nhfbnp = nhfbnp
  if(!is.null(option)) hfb$option = option
  
  
  if(hfb$nphfb > 0){
    # data set 2
    hfb$parnam = parnam
    hfb$partyp = rep('HFB', hfb$nphfb)
    hfb$parval = parval
    hfb$nlst = nlst
    
    # data set 3
    hfb$layer_parm = layer_parm
    hfb$irow1_parm = irow1_parm
    hfb$icol1_parm = icol1_parm
    hfb$irow2_parm = irow2_parm
    hfb$icol2_parm = icol2_parm
    hfb$factor_parm = factor_parm
  }
  
  # data set 4
  if(nhfbnp > 0){
    hfb$layer_noparm = layer_noparm
    hfb$irow1_noparm = irow1_noparm
    hfb$icol1_noparm = icol1_noparm
    hfb$irow2_noparm = irow2_noparm
    hfb$icol2_noparm = icol2_noparm
    hfb$hydchr_noparm = hydchr_noparm
  }
  
  # data set 5
  hfb$nacthfb = nacthfb
  
  # data set 6
  if(hfb$nacthfb > 0) hfb$pname = pname
  
  class(hfb) = c('hfb', 'rmf_package')
  return(hfb)
  
}
