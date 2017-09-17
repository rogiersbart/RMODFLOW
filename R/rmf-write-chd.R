#' Write a MODFLOW time-variant specified-head file
#'
#' \code{rmf_write_chd} writes a MODFLOW time-variant specified-head file based on an \code{RMODFLOW} chd object
#' 
#' @param chd an \code{RMODFLOW} chd object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.chd'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_chd}}, \code{\link{rmf_create_chd}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?chd.htm}

rmf_write_chd = function(chd, dis=read_dis(), file={cat('Please choose chd file to overwrite or provide new filename ...\n'); file.choose()}){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Time-Variant Specified-Head Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(chd)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(chd$npchd) && chd$npchd > 0) write_modflow_variables('PARAMETER', chd$npchd, chd$mxl, file=file)
  
  # data set 2
  write_modflow_variables(chd$mxactc, ifelse(chd$option == 'NOPRINT', 'NOPRINT', ''), ifelse((!is.null(chd$option) && (chd$option != 'NOPRINT')), paste(each='AUX', chd$option), ''), file=file)
  
  # parameters
  if(!is.null(chd$npchd) && chd$npchd > 0){
    for (i in 1:chd$npchd){
      
      # data set 3
      write_modflow_variables(chd$parnam[i], chd$partyp[i], chd$parval[i], chd$nlst[i], ifelse(chd$instances[i], 'INSTANCES', ''), ifelse(chd$instances[i], chd$numinst[i], ''), file=file)
      
      # time-varying
      if(!is.null(chd$instances) && chd$instances[i]){
        for (j in 1:chd$numinst[i]){
          
          # data set 4a
          write_modflow_variables(chd$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:chd$nlst[i]){
            
            write_modflow_variables(chd$layer_parm[[i]][j,k], chd$row_parm[[i]][j,k], chd$column_parm[[i]][j,k],  chd$shfact_parm[[i]][j,k], chd$ehdfactfact_parm[[i]][j,k], ifelse(!is.null(chd$xyz_parm[[i]][j,k]), chd$xyz_parm[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        for (k in 1:chd$nlst[i]){
          write_modflow_variables(chd$layer_parm[[i]][1,k], chd$row_parm[[i]][1,k], chd$column_parm[[i]][1,k], chd$shdfact_parm[[i]][1,k], chd$ehdfact_parm[[i]][1,k], ifelse(!is.null(chd$xyz_parm[[i]][1,k]), chd$xyz_parm[[i]][1,k], ''), file=file)
        }
      }  
      
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    write_modflow_variables(chd$itmp[i], chd$np[i], file=file)
    
    # data set 6
    if(chd$itmp[i] > 0){
      for(j in 1:chd$itmp[i]){
        write_modflow_variables(chd$layer_sp[[i]][j], chd$row_sp[[i]][j], chd$column_sp[[i]][j],  chd$shead_sp[[i]][j], chd$ehead_sp[[i]][j], ifelse(!is.null(chd$xyz_sp), chd$xyz_sp[[i]][j], ''), file=file)
      }
    }
    
    # data set 7
    if(chd$np[i] > 0){
      for(j in 1:chd$np[i]){
        write_modflow_variables(chd$pname[[i]][j], ifelse(!is.null(chd$instances) && chd$instances[which(chd$parnam==chd$pname[[i]][j])], chd$iname[[i]][j], ''), file=file)
      }
    }
  }
  
}