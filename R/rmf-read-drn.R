#' Read a MODFLOW drain file
#' 
#' \code{rmf_read_drn} reads in a MODFLOW drain file and returns it as an \code{RMODFLOW} drn object.
#'
#' @param file filename; typically '*.drn'
#' @param dis an \code{RMODFLOW} dis object
#' 
#' @return \code{RMODFLOW} drn object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_drn}}, \code{\link{rmf_create_drn}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?drn.htm}

rmf_read_drn = function(file = {cat('Please select drain file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}
){
  
  drn = list()
  drn_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(drn_lines)
  comment(drn) = data_set_0$comments
  drn_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(drn_lines)
  if('PARAMETER' %in% data_set_1$variables) {
    drn$npdrn = as.numeric(data_set_1$variables[2])
    drn$mxl = as.numeric(data_set_1$variables[3])
    drn_lines = data_set_1$remaining_lines
  }  
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(drn_lines)
  drn$mxactd = as.numeric(data_set_2$variables[1])
  drn$idrncb = as.numeric(data_set_2$variables[2])
  if(length(data_set_2$variables) > 2)   drn$option = c(as.character(data_set_2$variables[match(c('AUX', 'AUXILIARY'),data_set_2$variables)+1]), ifelse('NOPRINT' %in% data_set_2$variables, 'NOPRINT', ''))
  drn_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(!is.null(drn$npdrn) && drn$npdrn > 0){
    drn$xyz_parm = drn$condfact_parm = drn$elevation_parm = drn$column_parm = drn$row_parm = drn$layer_parm = list()
    
    i=1
    while(i <= drn$npdrn){
      # data set 3
      data_set_3 = rmfi_parse_variables(drn_lines)
      drn$parnam[i] =  as.character(data_set_3$variables[1])
      drn$partyp[i] =  'DRN'
      drn$parval[i] = as.numeric(data_set_3$variables[3])
      drn$nlst[i] = as.numeric(data_set_3$variables[4])
      if(length(data_set_3$variables) > 4){
        drn$instances[i]=T
        drn$numinst[i] = as.numeric(data_set_3$variables[6])
      } 
      drn_lines = data_set_3$remaining_lines
      rm(data_set_3)
      
      
      # time-varying parameters
      if(!is.null(drn$instances) && drn$instances[i]){
        drn$layer_parm[[i]] = drn$row_parm[[i]] = drn$column_parm[[i]] = drn$elevation_parm[[i]] = drn$condfact_parm[[i]] = drn$xyz_parm[[i]] = array(dim=c(drn$numinst[i],drn$nlst[i]))
        drn$instnam[[i]] = vector(mode='character', length=drn$numinst[i])
        
        j=1
        while(j <= drn$numinst[i]){
          # data set 4a
          data_set_4a = rmfi_parse_variables(drn_lines)
          drn$instnam[[i]][j] =  as.character(data_set_4a$variables)
          drn_lines = data_set_4a$remaining_lines
          rm(data_set_4a)
          
          k=1
          while(k <= drn$nlst[i]){
            
            # data set 4b
            data_set_4b = rmfi_parse_variables(drn_lines)
            drn$layer_parm[[i]][j,k] = as.numeric(data_set_4b$variables[1])
            drn$row_parm[[i]][j,k] = as.numeric(data_set_4b$variables[2])
            drn$column_parm[[i]][j,k] = as.numeric(data_set_4b$variables[3])
            drn$elevation_parm[[i]][j,k] = as.numeric(data_set_4b$variables[4])
            drn$condfact_parm[[i]][j,k] = as.numeric(data_set_4b$variables[5])

            if(length(data_set_4b$variables) > 5) drn$xyz_parm[[i]][j,k] = as.character(data_set_4b$variables[6])
            
            k=k+1
            drn_lines = data_set_4b$remaining_lines
            rm(data_set_4b)
          }
          j = j+1
        } 
        
      } else {
        # non time-varying
        drn$layer_parm[[i]] = drn$row_parm[[i]] = drn$column_parm[[i]] = drn$elevation_parm[[i]] = drn$condfact_parm[[i]] = drn$xyz_parm[[i]] = array(dim=c(1, drn$nlst[i]))
        
        k=1
        while(k <= drn$nlst[i]){
          # data set 4b
          data_set_4b = rmfi_parse_variables(drn_lines)
          drn$layer_parm[[i]][1,k] = as.numeric(data_set_4b$variables[1])
          drn$row_parm[[i]][1,k] = as.numeric(data_set_4b$variables[2])
          drn$column_parm[[i]][1,k] = as.numeric(data_set_4b$variables[3])
          drn$elevation_parm[[i]][1,k] = as.numeric(data_set_4b$variables[4])
          drn$condfact_parm[[i]][1,k] = as.numeric(data_set_4b$variables[5])

          if(length(data_set_4b$variables) > 5) drn$xyz_parm[[i]][1,k] = as.character(data_set_4b$variables[6])
          
          k=k+1
          drn_lines = data_set_4b$remaining_lines
          rm(data_set_4b)
        }
        
      }
      
      i = i+1
    }
    if(all(is.na(unlist(drn$xyz_parm)))) drn$xyz_parm = NULL
  }
  
  # stress periods
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 = rmfi_parse_variables(drn_lines)
    drn$itmp[i] = as.numeric(data_set_5$variables[1])
    drn$np[i] = as.numeric(data_set_5$variables[2])
    drn_lines = data_set_5$remaining_lines
    rm(data_set_5)
    
    if(drn$itmp[i] > 0){
      
      drn$xyz_sp[[i]] = drn$cond_sp[[i]] = drn$elevation_sp[[i]] = drn$column_sp[[i]] = drn$row_sp[[i]] = drn$layer_sp[[i]] = vector(length = drn$itmp[i])
      for(j in 1:drn$itmp[i]){
        # data set 6
        data_set_6 = rmfi_parse_variables(drn_lines)
        drn$layer_sp[[i]][j] = as.numeric(data_set_6$variables[1])
        drn$row_sp[[i]][j] = as.numeric(data_set_6$variables[2])
        drn$column_sp[[i]][j] = as.numeric(data_set_6$variables[3])
        drn$elevation_sp[[i]][j] = as.numeric(data_set_6$variables[4])
        drn$cond_sp[[i]][j] = as.numeric(data_set_6$variables[5])

        if(length(data_set_6$variables) > 5) drn$xyz_sp[[i]][j] = as.character(data_set_6$variables[6])
        
        drn_lines = data_set_6$remaining_lines
        rm(data_set_6)
      } 
    } 
    if(is.logical(unlist(drn$xyz_sp)) && !any(unlist(drn$xyz_sp))) drn$xyz_sp = NULL
    
    if(drn$np[i] > 0){
      drn$iname[[i]] = drn$pname[[i]] = vector(length=drn$np[i])
      for(j in 1:drn$np[i]){
        # data set 7
        data_set_7 = rmfi_parse_variables(drn_lines)
        drn$pname[[i]][j] = as.character(data_set_7$variables[1])
        if(length(data_set_7$variables) > 1) drn$iname[[i]][j] = as.character(data_set_7$variables[2])
        
        drn_lines = data_set_7$remaining_lines
        rm(data_set_7)
      }
    }
  }
  if(is.logical(unlist(drn$iname)) && !any(unlist(drn$iname))) drn$iname = NULL
  
  class(drn) = c('drn', 'rmf_package')
  return(drn)
  
}
