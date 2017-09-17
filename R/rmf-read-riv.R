#' Read a MODFLOW river file
#' 
#' \code{rmf_read_riv} reads in a MODFLOW river file and returns it as an \code{RMODFLOW} riv object.
#'
#' @param file filename; typically '*.riv'
#' @param dis an \code{RMODFLOW} dis object
#' 
#' @return \code{RMODFLOW} riv object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_riv}}, \code{\link{rmf_create_riv}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?riv.htm}

rmf_read_riv = function(file = {cat('Please select river file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}
){
  
  riv = list()
  riv_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(riv_lines)
  comment(riv) = data_set_0$comments
  riv_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(riv_lines)
  if('PARAMETER' %in% data_set_1$variables) {
    riv$npriv = as.numeric(data_set_1$variables[2])
    riv$mxl = as.numeric(data_set_1$variables[3])
    riv_lines = data_set_1$remaining_lines
  }  
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(riv_lines)
  riv$mxactr = as.numeric(data_set_2$variables[1])
  riv$irivcb = as.numeric(data_set_2$variables[2])
  if(length(data_set_2$variables) > 2)   riv$option = c(as.character(data_set_2$variables[match(c('AUX', 'AUXILIARY'),data_set_2$variables)+1]), ifelse('NOPRINT' %in% data_set_2$variables, 'NOPRINT', ''))
  riv_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(!is.null(riv$npriv) && riv$npriv > 0){
    riv$xyz_parm = riv$rbot_parm = riv$condfact_parm = riv$stage_parm = riv$column_parm = riv$row_parm = riv$layer_parm = list()
    
    i=1
    while(i <= riv$npriv){
      # data set 3
      data_set_3 = rmfi_parse_variables(riv_lines)
      riv$parnam[i] =  as.character(data_set_3$variables[1])
      riv$partyp[i] =  'RIV'
      riv$parval[i] = as.numeric(data_set_3$variables[3])
      riv$nlst[i] = as.numeric(data_set_3$variables[4])
      if(length(data_set_3$variables) > 4){
        riv$instances[i]=T
        riv$numinst[i] = as.numeric(data_set_3$variables[6])
      } 
      riv_lines = data_set_3$remaining_lines
      rm(data_set_3)
      
      
      # time-varying parameters
      if(!is.null(riv$instances) && riv$instances[i]){
        riv$layer_parm[[i]] = riv$row_parm[[i]] = riv$column_parm[[i]] = riv$stage_parm[[i]] = riv$condfact_parm[[i]] = riv$rbot_parm[[i]] = riv$xyz_parm[[i]] = array(dim=c(riv$numinst[i],riv$nlst[i]))
        riv$instnam[[i]] = vector(mode='character', length=riv$numinst[i])
        
        j=1
        while(j <= riv$numinst[i]){
          # data set 4a
          data_set_4a = rmfi_parse_variables(riv_lines)
          riv$instnam[[i]][j] =  as.character(data_set_4a$variables)
          riv_lines = data_set_4a$remaining_lines
          rm(data_set_4a)
          
          k=1
          while(k <= riv$nlst[i]){
            
            # data set 4b
            data_set_4b = rmfi_parse_variables(riv_lines)
            riv$layer_parm[[i]][j,k] = as.numeric(data_set_4b$variables[1])
            riv$row_parm[[i]][j,k] = as.numeric(data_set_4b$variables[2])
            riv$column_parm[[i]][j,k] = as.numeric(data_set_4b$variables[3])
            riv$stage_parm[[i]][j,k] = as.numeric(data_set_4b$variables[4])
            riv$condfact_parm[[i]][j,k] = as.numeric(data_set_4b$variables[5])
            riv$rbot_parm[[i]][j,k] = as.numeric(data_set_4b$variables[6])
            
            if(length(data_set_4b$variables) > 6) riv$xyz_parm[[i]][j,k] = as.character(data_set_4b$variables[7])
            
            k=k+1
            riv_lines = data_set_4b$remaining_lines
            rm(data_set_4b)
          }
          j = j+1
        } 
        
      } else {
        # non time-varying
        riv$layer_parm[[i]] = riv$row_parm[[i]] = riv$column_parm[[i]] = riv$stage_parm[[i]] = riv$condfact_parm[[i]] = riv$rbot_parm[[i]] = riv$xyz_parm[[i]] = array(dim=c(1, riv$nlst[i]))
        
        k=1
        while(k <= riv$nlst[i]){
          # data set 4b
          data_set_4b = rmfi_parse_variables(riv_lines)
          riv$layer_parm[[i]][1,k] = as.numeric(data_set_4b$variables[1])
          riv$row_parm[[i]][1,k] = as.numeric(data_set_4b$variables[2])
          riv$column_parm[[i]][1,k] = as.numeric(data_set_4b$variables[3])
          riv$stage_parm[[i]][1,k] = as.numeric(data_set_4b$variables[4])
          riv$condfact_parm[[i]][1,k] = as.numeric(data_set_4b$variables[5])
          riv$rbot_parm[[i]][1,k] = as.numeric(data_set_4b$variables[6])
          
          if(length(data_set_4b$variables) > 6) riv$xyz_parm[[i]][1,k] = as.character(data_set_4b$variables[7])
          
          k=k+1
          riv_lines = data_set_4b$remaining_lines
          rm(data_set_4b)
        }
        
      }
      
      i = i+1
    }
    if(all(is.na(unlist(riv$xyz_parm)))) riv$xyz_parm = NULL
  }
  
  # stress periods
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 = rmfi_parse_variables(riv_lines)
    riv$itmp[i] = as.numeric(data_set_5$variables[1])
    riv$np[i] = as.numeric(data_set_5$variables[2])
    riv_lines = data_set_5$remaining_lines
    rm(data_set_5)
    
    if(riv$itmp[i] > 0){
      
      riv$xyz_sp[[i]] = riv$rbot_sp[[i]] = riv$cond_sp[[i]] = riv$stage_sp[[i]] = riv$column_sp[[i]] = riv$row_sp[[i]] = riv$layer_sp[[i]] = vector(length = riv$itmp[i])
      for(j in 1:riv$itmp[i]){
        # data set 6
        data_set_6 = rmfi_parse_variables(riv_lines)
        riv$layer_sp[[i]][j] = as.numeric(data_set_6$variables[1])
        riv$row_sp[[i]][j] = as.numeric(data_set_6$variables[2])
        riv$column_sp[[i]][j] = as.numeric(data_set_6$variables[3])
        riv$stage_sp[[i]][j] = as.numeric(data_set_6$variables[4])
        riv$cond_sp[[i]][j] = as.numeric(data_set_6$variables[5])
        riv$rbot_sp[[i]][j] = as.numeric(data_set_6$variables[6])
        
        if(length(data_set_6$variables) > 6) riv$xyz_sp[[i]][j] = as.character(data_set_6$variables[7])
        
        riv_lines = data_set_6$remaining_lines
        rm(data_set_6)
      } 
    } 
    if(is.logical(unlist(riv$xyz_sp)) && !any(unlist(riv$xyz_sp))) riv$xyz_sp = NULL
    
    if(riv$np[i] > 0){
      riv$iname[[i]] = riv$pname[[i]] = vector(length=riv$np[i])
      for(j in 1:riv$np[i]){
        # data set 7
        data_set_7 = rmfi_parse_variables(riv_lines)
        riv$pname[[i]][j] = as.character(data_set_7$variables[1])
        if(length(data_set_7$variables) > 1) riv$iname[[i]][j] = as.character(data_set_7$variables[2])
        
        riv_lines = data_set_7$remaining_lines
        rm(data_set_7)
      }
    }
  }
  if(is.logical(unlist(riv$iname)) && !any(unlist(riv$iname))) riv$iname = NULL
  
  class(riv) = c('riv', 'rmf_package')
  return(riv)
  
}
