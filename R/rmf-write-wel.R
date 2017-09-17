#' Write a MODFLOW well file
#'
#' \code{rmf_write_wel} writes a MODFLOW well file based on an \code{RMODFLOW} wel object
#' 
#' @param wel an \code{RMODFLOW} wel object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.wel'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_wel}}, \code{\link{rmf_create_wel}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?wel.htm}

rmf_write_wel = function(wel, dis=read_dis(), file={cat('Please choose wel file to overwrite or provide new filename ...\n'); file.choose()}){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Well Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(wel)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(wel$npwel) && wel$npwel > 0) rmfi_write_variables('PARAMETER', wel$npwel, wel$mxl, file=file)
  
  # data set 2
  rmfi_write_variables(wel$mxactw, wel$iwelcb, ifelse(wel$option == 'NOPRINT', 'NOPRINT', ''), ifelse((!is.null(wel$option) && (wel$option != 'NOPRINT')), paste(each='AUX', wel$option), ''), file=file)
  
  # parameters
  if(!is.null(wel$npwel) && wel$npwel > 0){
    for (i in 1:wel$npwel){
      
      # data set 3
      rmfi_write_variables(wel$parnam[i], wel$partyp[i], wel$parval[i], wel$nlst[i], ifelse(wel$instances[i], 'INSTANCES', ''), ifelse(wel$instances[i], wel$numinst[i], ''), file=file)
      
      # time-varying
      if(!is.null(wel$instances) && wel$instances[i]){
        for (j in 1:wel$numinst[i]){
          
          # data set 4a
          rmfi_write_variables(wel$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:wel$nlst[i]){
            
            rmfi_write_variables(wel$layer_parm[[i]][j,k], wel$row_parm[[i]][j,k], wel$column_parm[[i]][j,k],  wel$qfact_parm[[i]][j,k], ifelse(!is.null(wel$xyz_parm[[i]][j,k]), wel$xyz_parm[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        for (k in 1:wel$nlst[i]){
        rmfi_write_variables(wel$layer_parm[[i]][1,k], wel$row_parm[[i]][1,k], wel$column_parm[[i]][1,k],  wel$qfact_parm[[i]][1,k], ifelse(!is.null(wel$xyz_parm[[i]][1,k]), wel$xyz_parm[[i]][1,k], ''), file=file)
        }
      }  
   
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    rmfi_write_variables(wel$itmp[i], wel$np[i], file=file)
    
    # data set 6
    if(wel$itmp[i] > 0){
      for(j in 1:wel$itmp[i]){
        rmfi_write_variables(wel$layer_sp[[i]][j], wel$row_sp[[i]][j], wel$column_sp[[i]][j],  wel$q_sp[[i]][j], ifelse(!is.null(wel$xyz_sp), wel$xyz_sp[[i]][j], ''), file=file)
      }
    }
    
    # data set 7
    if(wel$np[i] > 0){
      for(j in 1:wel$np[i]){
        rmfi_write_variables(wel$pname[[i]][j], ifelse(!is.null(wel$instances) && wel$instances[which(wel$parnam==wel$pname[[i]][j])], wel$iname[[i]][j], ''), file=file)
      }
    }
  }
  
}