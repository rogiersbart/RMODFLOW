#' Write a MODFLOW evapotranspiration file
#'
#' \code{rmf_write_evt} writes a MODFLOW evapotranspiration file based on an \code{RMODFLOW} evt object
#' 
#' @param evt an \code{RMODFLOW} evt object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.evt'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_evt}}, \code{\link{rmf_create_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}


rmf_write_evt = function(evt, dis=rmf_read_dis(), file = {cat('Please select evt file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Evapotranspiration Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(evt)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(evt$npevt) && evt$npevt > 0 ) rmfi_write_variables('PARAMETER', evt$npevt, file=file)
  
  # data set 2
  rmfi_write_variables(evt$nevtop, evt$ievtcb, file=file)
  
  # parameters
  if(!is.null(evt$npevt) && evt$npevt > 0){
    for (i in 1:evt$npevt){
      # data set 3
      rmfi_write_variables(evt$parnam[i], evt$partyp[i], evt$parval[i], evt$nclu[i], ifelse(evt$instances[i], 'INSTANCES', ' '), ifelse(evt$instances[i], evt$numinst[i], ' '), file=file)
      
      # time-varying
      if(!is.null(evt$instances) && evt$instances[i]){
        for (j in 1:evt$numinst[i]){
          # data set 4a
          if(evt$instances[i]) rmfi_write_variables(evt$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:evt$nclu[i]){
            rmfi_write_variables(evt$mltarr[[i]][j, k], evt$zonarr[[i]][j,k], ifelse(!is.null(evt$iz) && evt$zonarr[[i]][j,k]!='ALL', evt$iz[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        # data set 4b
        for (k in 1:evt$nclu[i]){
          rmfi_write_variables(evt$mltarr[[i]][1, k], evt$zonarr[[i]][1,k], ifelse(!is.null(evt$iz) && evt$zonarr[[i]][1,k]!='ALL', evt$iz[[i]][1,k], ''), file=file)
          
        }
      }
      
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    rmfi_write_variables(evt$insurf[i], evt$inevtr[i], evt$inexdp[i], ifelse(evt$nevtop == 2, evt$inievt[i], ''), file=file)
    
    # data set 6
    if(evt$insurf[i] >= 0) rmfi_write_array(evt$surf[,,i], file=file)
    
    # data set 7
    if(((!is.null(evt$npevt) && evt$npevt==0) || is.null(evt$npevt)) && evt$inevtr[i] >= 0) rmfi_write_array(evt$evtr[,,i], file=file)
    
    # data set 8
    if((!is.null(evt$npevt) && evt$npevt > 0) && evt$inevtr[i] > 0){
      for (j in 1:evt$inevtr[i]){
        
        rmfi_write_variables(evt$pname[[i]][j], ifelse(!is.null(evt$instances) && evt$instances[which(evt$parnam==evt$pname[[i]][j])], evt$iname[[i]][j], ''), ifelse((is.null(evt$instances) || (!is.null(evt$instances) && !(evt$instances[which(evt$parnam==evt$pname[[i]][j])]))) && !is.null(evt$ievtpf), evt$ievtpf[[i]][j], ' '), file=file) 
        
      }
    }
    
    # data set 9
    if(evt$inexdp[i] >= 0) rmfi_write_array(evt$exdp[,,i], file=file)
    
    # data set 10
    if(evt$nevtop==2 && (!is.null(evt$inievt) && evt$inievt[i] >= 0)){
      rmfi_write_array(evt$ievt[,,i], file=file)
    }
    
  }
  
  
}