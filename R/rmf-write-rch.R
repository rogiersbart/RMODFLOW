#' Write a MODFLOW recharge file
#'
#' \code{rmf_write_rch} writes a MODFLOW recharge file based on an \code{RMODFLOW} rch object
#' 
#' @param rch an \code{RMODFLOW} rch object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.rch'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_rch}}, \code{\link{rmf_create_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}


rmf_write_rch <-  function(rch, dis = rmf_read_dis(), file={cat('Please choose rch file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste(paste('# MODFLOW Recharge Package created by RMODFLOW, version'),v,'\n'), file = file)
  cat(paste('#', comment(rch)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(rch$dimensions$np > 0) rmfi_write_variables('PARAMETER', rch$dimensions$np, file=file)
  
  # data set 2
  rmfi_write_variables(rch$nrchop, rch$irchcb, file=file, ...)
  
  partyp <- 'RCH'
  
  # parameters
  if(rch$dimensions$np > 0) rmfi_write_array_parameters(obj = rch, arrays = rch$recharge, file = file, partyp = 'RCH', type = 'bc', ...)
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    # inrech
    names_act <- colnames(rch$kper)[which(rch$kper[i,which(!is.na(rch$kper[i,]))] != FALSE)[-1]]
    if(i > 1 && identical(names_act, colnames(rch$kper)[which(rch$kper[i-1,which(!is.na(rch$kper[i-1,]))] != FALSE)[-1]])) {
      inrech <- -1
    } else {
      inrech <- length(names_act)
    }
    # inirch
    inirch <- 0
    if(rch$nrchop == 2) {
      irch_act <-  colnames(rch$irch_kper)[which(rch$irch_kper[i,which(!is.na(rch$irch_kper[i,]))] != FALSE)[-1]]
      if(length(irch_act) > 0) {
        if(i > 1 && identical(irch_act, colnames(rch$irch_kper)[which(rch$irch_kper[i-1,which(!is.na(rch$irch_kper[i-1,]))] != FALSE)[-1]])) {
          inirch <- -1
        } else {
          inirch <- length(irch_act)
        }
      } 
    }
  
    if(rch$dimensions$np > 0) {
      parm_names_active <- parm_names[parm_names %in% names_act]
      np <- length(parm_names_active)
    } else {
      np <- 0
    }
    
    rmfi_write_variables(inrech, ifelse(rch$nrchop == 2, inirch, ''), file=file, ...)
    
    # data set 6
    if(np == 0 && inrech >= 0) rmfi_write_array(rch$recharge[[names_act]], file = file, ...)
    
    # data set 7
    if(np > 0){
      for(j in 1:np){
        rmfi_write_variables(parm_names_active[j], ifelse(tv_parm[j], rch$kper[i,parm_names_active[j]], ''), ifelse(length(rch$irchpf) == 1, rch$irchpf, rch$irchpf[j]), file=file)
      }
    }
    
    # data set 8
    if(rch$nrchop == 2 && inirch >= 0) {
      irch_act <- which(c(rch$irch_kper[i,-1]))
      rmfi_write_array(rch$irch[[irch_act]], file = file, ...)
    }
  }
  
}
