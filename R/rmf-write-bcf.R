#' Write a MODFLOW block-centered flow file
#' 
#' \code{rmf_write_bcf} writes a MODFLOW block-centered flow file based on an \code{RMODFLOW} bcf object
#'
#' @param bcf an \code{RMODFLOW} bcf object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.bcf'
#' @param ... arguments passed to \code{rmfi_write_array} and \code{rmfi_write_variables}. Can be ignored when format is free and arrays are INTERNAL or CONSTANT.
#'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_bcf}}, \code{\link{rmf_create_bcf}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}

rmf_write_bcf = function(bcf, dis = rmf_read_dis(), file = {cat('Please select bcf file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  # data set 0
  # v <- packageDescription("RMODFLOW")$Version
  # cat(paste('# MODFLOW Block-Centered Flow Package created by RMODFLOW, version',v,'\n'), file = file)
  # cat(paste('#', comment(bcf)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(bcf$ibcfcb, bcf$hdry, bcf$iwdflg, bcf$wetfct, bcf$iwetit, bcf$ihdwet, file = file, ...)
  
  # data set 2 - should write 40 characters if format is fixed
  fmt <- ifelse('format' %in% names(list(...)), get('format'), 'free')
  ltype <- paste(bcf$layavg, bcf$laycon, sep='')
  
  if(fmt == 'free') {
    rmfi_write_variables(ltype, file=file)
  } else if(fmt == "fixed") {
      counter <- 0
      while((dis$nlay-counter) > 20) {
        rmfi_write_variables(paste0(ltype[1:20], collapse=''), file=file)
        ltype <- ltype[-c(1:20)]
        counter <- counter+20
      }
      rmfi_write_variables(paste0(ltype, collapse=''), file=file)
  }
  
  # data set 3
  rmfi_write_array(bcf$trpy, file=file, ...)
  
  # data set 4-9
  for(i in 1:dis$nlay){
    if('TR' %in% dis$sstr) rmfi_write_array(bcf$sf1[,,i], file=file, ...)
    if(bcf$laycon[i] %in% c(0,2)) rmfi_write_array(bcf$tran[,,i], file=file, ...)
    if(bcf$laycon[i] %in% c(1,3)) rmfi_write_array(bcf$hy[,,i], file=file, ...)
    if(i != dis$nlay) rmfi_write_array(bcf$vcont[,,i], file=file, ...)
    if(('TR' %in% dis$sstr) && bcf$laycon[i] %in% c(2,3)) rmfi_write_array(bcf$sf2[,,i], file=file, ...)
    if((bcf$iwdflg != 0) && (bcf$laycon[i] %in% c(1,3))) rmfi_write_array(bcf$wetdry[,,i], file=file, ...)
  }
}