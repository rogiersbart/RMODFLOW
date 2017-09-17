#' Write a MODFLOW block-centered flow file
#' 
#' \code{rmf_write_bcf} writes a MODFLOW block-centered flow file based on an \code{RMODFLOW} bcf object
#'
#' @param bcf an \code{RMODFLOW} bcf object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.bcf'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_bcf}}, \code{\link{rmf_create_bcf}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}

rmf_write_bcf = function(bcf, dis = rmf_read_dis(), file = {cat('Please select bcf file to overwrite or provide new filename ...\n'); file.choose()}){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Block-Centered Flow Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(bcf)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  write_modflow_variables(bcf$ibcfcb, bcf$hdry, bcf$iwdflg, bcf$wetfct, bcf$iwetit, bcf$ihdwet, file = file)
  
  # data set 2
  for(i in 1:dis$nlay){
    write_modflow_variables(bcf$int_trans[i], bcf$laycon[i], file=file)
  }
  
  # data set 3
  write_modflow_variables(bcf$trpy, file=file)
  
  # data set 4-9
  for(i in 1:dis$nlay){
    if('TR' %in% dis$sstr) write_modflow_array(bcf$sf1[,,i], file=file)
    if(bcf$laycon[i] %in% c(0,2)) write_modflow_array(bcf$tran[,,i], file=file)
    if(bcf$laycon[i] %in% c(1,3)) write_modflow_array(bcf$hy[,,i], file=file)
    if(i != dis$nlay) write_modflow_array(bcf$vcont[,,i], file=file)
    if(('TR' %in% dis$sstr) && bcf$laycon[i] %in% c(2,3)) write_modflow_array(bcf$sf2[,,i], file=file)
    if((bcf$iwdflg != 0) && (bcf$laycon[i] %in% c(1,3))) write_modflow_array(bcf$wetdry[,,i], file=file)
  }
}