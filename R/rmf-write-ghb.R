#' Write a MODFLOW general-head boundary file
#'
#' \code{rmf_write_ghb} writes a MODFLOW general-head boundary file based on an \code{RMODFLOW} ghb object
#' 
#' @param ghb an \code{RMODFLOW} ghb object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.ghb'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_ghb}}, \code{\link{rmf_create_ghb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?ghb.htm}

rmf_write_ghb = function(ghb, dis=read_dis(), file={cat('Please choose ghb file to overwrite or provide new filename ...\n'); file.choose()}){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW General-Head Boundary Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(ghb)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(ghb$npghb) && ghb$npghb > 0) write_modflow_variables('PARAMETER', ghb$npghb, ghb$mxl, file=file)
  
  # data set 2
  write_modflow_variables(ghb$mxactb, ghb$ighbcb, ifelse(ghb$option == 'NOPRINT', 'NOPRINT', ''), ifelse((!is.null(ghb$option) && (ghb$option != 'NOPRINT')), paste(each='AUX', ghb$option), ''), file=file)
  
  # parameters
  if(!is.null(ghb$npghb) && ghb$npghb > 0){
    for (i in 1:ghb$npghb){
      
      # data set 3
      write_modflow_variables(ghb$parnam[i], ghb$partyp[i], ghb$parval[i], ghb$nlst[i], ifelse(ghb$instances[i], 'INSTANCES', ''), ifelse(ghb$instances[i], ghb$numinst[i], ''), file=file)
      
      # time-varying
      if(!is.null(ghb$instances) && ghb$instances[i]){
        for (j in 1:ghb$numinst[i]){
          
          # data set 4a
          write_modflow_variables(ghb$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:ghb$nlst[i]){
            
            write_modflow_variables(ghb$layer_parm[[i]][j,k], ghb$row_parm[[i]][j,k], ghb$column_parm[[i]][j,k],  ghb$bhead_parm[[i]][j,k], ghb$condfact_parm[[i]][j,k], ifelse(!is.null(ghb$xyz_parm[[i]][j,k]), ghb$xyz_parm[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        for (k in 1:ghb$nlst[i]){
          write_modflow_variables(ghb$layer_parm[[i]][1,k], ghb$row_parm[[i]][1,k], ghb$column_parm[[i]][1,k], ghb$bhead_parm[[i]][1,k], ghb$condfact_parm[[i]][1,k], ifelse(!is.null(ghb$xyz_parm[[i]][1,k]), ghb$xyz_parm[[i]][1,k], ''), file=file)
        }
      }  
      
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    write_modflow_variables(ghb$itmp[i], ghb$np[i], file=file)
    
    # data set 6
    if(ghb$itmp[i] > 0){
      for(j in 1:ghb$itmp[i]){
        write_modflow_variables(ghb$layer_sp[[i]][j], ghb$row_sp[[i]][j], ghb$column_sp[[i]][j],  ghb$bhead_sp[[i]][j], ghb$cond_sp[[i]][j], ifelse(!is.null(ghb$xyz_sp), ghb$xyz_sp[[i]][j], ''), file=file)
      }
    }
    
    # data set 7
    if(ghb$np[i] > 0){
      for(j in 1:ghb$np[i]){
        write_modflow_variables(ghb$pname[[i]][j], ifelse(!is.null(ghb$instances) && ghb$instances[which(ghb$parnam==ghb$pname[[i]][j])], ghb$iname[[i]][j], ''), file=file)
      }
    }
  }
  
}