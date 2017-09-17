#' Write a MODFLOW river file
#'
#' \code{rmf_write_riv} writes a MODFLOW river file based on an \code{RMODFLOW} riv object
#' 
#' @param riv an \code{RMODFLOW} riv object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.riv'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_riv}}, \code{\link{rmf_create_riv}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?riv.htm}

rmf_write_riv = function(riv, dis=read_dis(), file={cat('Please choose riv file to overwrite or provide new filename ...\n'); file.choose()}){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW River Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(riv)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(riv$npriv) && riv$npriv > 0) rmfi_write_variables('PARAMETER', riv$npriv, riv$mxl, file=file)
  
  # data set 2
  rmfi_write_variables(riv$mxactr, riv$irivcb, ifelse(riv$option == 'NOPRINT', 'NOPRINT', ''), ifelse((!is.null(riv$option) && (riv$option != 'NOPRINT')), paste(each='AUX', riv$option), ''), file=file)
  
  # parameters
  if(!is.null(riv$npriv) && riv$npriv > 0){
    for (i in 1:riv$npriv){
      
      # data set 3
      rmfi_write_variables(riv$parnam[i], riv$partyp[i], riv$parval[i], riv$nlst[i], ifelse(riv$instances[i], 'INSTANCES', ''), ifelse(riv$instances[i], riv$numinst[i], ''), file=file)
      
      # time-varying
      if(!is.null(riv$instances) && riv$instances[i]){
        for (j in 1:riv$numinst[i]){
          
          # data set 4a
          rmfi_write_variables(riv$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:riv$nlst[i]){
            
            rmfi_write_variables(riv$layer_parm[[i]][j,k], riv$row_parm[[i]][j,k], riv$column_parm[[i]][j,k],  riv$stage_parm[[i]][j,k], riv$condfact_parm[[i]][j,k], riv$rbot_parm[[i]][j,k], ifelse(!is.null(riv$xyz_parm[[i]][j,k]), riv$xyz_parm[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        for (k in 1:riv$nlst[i]){
          rmfi_write_variables(riv$layer_parm[[i]][1,k], riv$row_parm[[i]][1,k], riv$column_parm[[i]][1,k], riv$stage_parm[[i]][1,k], riv$condfact_parm[[i]][1,k], riv$rbot_parm[[i]][1,k], ifelse(!is.null(riv$xyz_parm[[i]][1,k]), riv$xyz_parm[[i]][1,k], ''), file=file)
        }
      }  
      
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    rmfi_write_variables(riv$itmp[i], riv$np[i], file=file)
    
    # data set 6
    if(riv$itmp[i] > 0){
      for(j in 1:riv$itmp[i]){
        rmfi_write_variables(riv$layer_sp[[i]][j], riv$row_sp[[i]][j], riv$column_sp[[i]][j],  riv$stage_sp[[i]][j], riv$cond_sp[[i]][j], riv$rbot_sp[[i]][j], ifelse(!is.null(riv$xyz_sp), riv$xyz_sp[[i]][j], ''), file=file)
      }
    }
    
    # data set 7
    if(riv$np[i] > 0){
      for(j in 1:riv$np[i]){
        rmfi_write_variables(riv$pname[[i]][j], ifelse(!is.null(riv$instances) && riv$instances[which(riv$parnam==riv$pname[[i]][j])], riv$iname[[i]][j], ''), file=file)
      }
    }
  }
  
}