#' Write a MODFLOW drain file
#'
#' \code{rmf_write_drn} writes a MODFLOW drain file based on an \code{RMODFLOW} drn object
#' 
#' @param drn an \code{RMODFLOW} drn object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.drn'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_drn}}, \code{\link{rmf_create_drn}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?drn.htm}

rmf_write_drn = function(drn, dis=read_dis(), file={cat('Please choose drn file to overwrite or provide new filename ...\n'); file.choose()}){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Drain Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(drn)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(drn$npdrn) && drn$npdrn > 0) write_modflow_variables('PARAMETER', drn$npdrn, drn$mxl, file=file)
  
  # data set 2
  write_modflow_variables(drn$mxactd, drn$idrncb, ifelse(drn$option == 'NOPRINT', 'NOPRINT', ''), ifelse((!is.null(drn$option) && (drn$option != 'NOPRINT')), paste(each='AUX', drn$option), ''), file=file)
  
  # parameters
  if(!is.null(drn$npdrn) && drn$npdrn > 0){
    for (i in 1:drn$npdrn){
      
      # data set 3
      write_modflow_variables(drn$parnam[i], drn$partyp[i], drn$parval[i], drn$nlst[i], ifelse(drn$instances[i], 'INSTANCES', ''), ifelse(drn$instances[i], drn$numinst[i], ''), file=file)
      
      # time-varying
      if(!is.null(drn$instances) && drn$instances[i]){
        for (j in 1:drn$numinst[i]){
          
          # data set 4a
          write_modflow_variables(drn$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:drn$nlst[i]){
            
            write_modflow_variables(drn$layer_parm[[i]][j,k], drn$row_parm[[i]][j,k], drn$column_parm[[i]][j,k],  drn$elevation_parm[[i]][j,k], drn$condfact_parm[[i]][j,k], ifelse(!is.null(drn$xyz_parm[[i]][j,k]), drn$xyz_parm[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        for (k in 1:drn$nlst[i]){
          write_modflow_variables(drn$layer_parm[[i]][1,k], drn$row_parm[[i]][1,k], drn$column_parm[[i]][1,k], drn$elevation_parm[[i]][1,k], drn$condfact_parm[[i]][1,k], ifelse(!is.null(drn$xyz_parm[[i]][1,k]), drn$xyz_parm[[i]][1,k], ''), file=file)
        }
      }  
      
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    write_modflow_variables(drn$itmp[i], drn$np[i], file=file)
    
    # data set 6
    if(drn$itmp[i] > 0){
      for(j in 1:drn$itmp[i]){
        write_modflow_variables(drn$layer_sp[[i]][j], drn$row_sp[[i]][j], drn$column_sp[[i]][j],  drn$elevation_sp[[i]][j], drn$cond_sp[[i]][j], ifelse(!is.null(drn$xyz_sp), drn$xyz_sp[[i]][j], ''), file=file)
      }
    }
    
    # data set 7
    if(drn$np[i] > 0){
      for(j in 1:drn$np[i]){
        write_modflow_variables(drn$pname[[i]][j], ifelse(!is.null(drn$instances) && drn$instances[which(drn$parnam==drn$pname[[i]][j])], drn$iname[[i]][j], ''), file=file)
      }
    }
  }
  
}