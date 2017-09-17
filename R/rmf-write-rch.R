#' Write a MODFLOW recharge file
#'
#' \code{rmf_write_rch} writes a MODFLOW recharge file based on an \code{RMODFLOW} rch object
#' 
#' @param rch an \code{RMODFLOW} rch object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.rch'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_rch}}, \code{\link{rmf_create_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}


rmf_write_rch = function(rch, dis=rmf_read_dis(), file = {cat('Please select rch file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Recharge Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(rch)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(rch$nprch) && rch$nprch > 0 ) rmfi_write_variables('PARAMETER', rch$nprch, file=file)
    
  # data set 2
  rmfi_write_variables(rch$nrchop, rch$irchcb, file=file)
  
    # parameters
  if(!is.null(rch$nprch) && rch$nprch > 0){
    for (i in 1:rch$nprch){
      # data set 3
      rmfi_write_variables(rch$parnam[i], rch$partyp[i], rch$parval[i], rch$nclu[i], ifelse(rch$instances[i], 'INSTANCES', ' '), ifelse(rch$instances[i], rch$numinst[i], ' '), file=file)
      
      # time-varying
      if(!is.null(rch$instances) && rch$instances[i]){
        for (j in 1:rch$numinst[i]){
          # data set 4a
          if(rch$instances[i]) rmfi_write_variables(rch$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:rch$nclu[i]){
            rmfi_write_variables(rch$mltarr[[i]][j, k], rch$zonarr[[i]][j,k], ifelse(!is.null(rch$iz) && rch$zonarr[[i]][j,k]!='ALL', rch$iz[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        # data set 4b
        for (k in 1:rch$nclu[i]){
          rmfi_write_variables(rch$mltarr[[i]][1, k], rch$zonarr[[i]][1,k], ifelse(!is.null(rch$iz) && rch$zonarr[[i]][1,k]!='ALL', rch$iz[[i]][1,k], ''), file=file)
          
        }
      }

    }
  }
 
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    rmfi_write_variables(rch$inrech[i], rch$inirch[i], file=file)
    
    # data set 6
    if(((!is.null(rch$nprch) && rch$nprch==0) || is.null(rch$nprch)) && rch$inrech[i] >= 0) rmfi_write_array(rch$rech[,,i], file=file)
    
    # data set 7
    if((!is.null(rch$nprch) && rch$nprch > 0) && rch$inrech[i] > 0){
      for (j in 1:rch$inrech[i]){
        
        rmfi_write_variables(rch$pname[[i]][j], ifelse(!is.null(rch$instances) && rch$instances[which(rch$parnam==rch$pname[[i]][j])], rch$iname[[i]][j], ''), ifelse((is.null(rch$instances) || (!is.null(rch$instances) && !(rch$instances[which(rch$parnam==rch$pname[[i]][j])]))) && !is.null(rch$irchpf), rch$irchpf[[i]][j], ' '), file=file) 
        
      }
    }
    
    # data set 8
    if(rch$nrchop==2 && (!is.null(rch$inirch) && rch$inirch[i] >= 0)){
      rmfi_write_array(rch$irch[,,i], file=file)
    }
    
  }
  
  
}