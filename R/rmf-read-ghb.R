#' Read a MODFLOW general-head boundary file
#' 
#' \code{rmf_read_ghb} reads in a MODFLOW general-head boundary file and returns it as an \code{RMODFLOW} ghb object.
#'
#' @param file filename; typically '*.ghb'
#' @param dis an \code{RMODFLOW} dis object
#' 
#' @return \code{RMODFLOW} ghb object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_ghb}}, \code{\link{rmf_create_ghb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?ghb.htm}

rmf_read_ghb = function(file = {cat('Please select general-head boundary file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}
){
  
  ghb = list()
  ghb_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(ghb_lines)
  comment(ghb) = data_set_0$comments
  ghb_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(ghb_lines)
  if('PARAMETER' %in% data_set_1$variables) {
    ghb$npghb = as.numeric(data_set_1$variables[2])
    ghb$mxl = as.numeric(data_set_1$variables[3])
    ghb_lines = data_set_1$remaining_lines
  }  
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(ghb_lines)
  ghb$mxactb = as.numeric(data_set_2$variables[1])
  ghb$ighbcb = as.numeric(data_set_2$variables[2])
  if(length(data_set_2$variables) > 2)   ghb$option = c(as.character(data_set_2$variables[match(c('AUX', 'AUXILIARY'),data_set_2$variables)+1]), ifelse('NOPRINT' %in% data_set_2$variables, 'NOPRINT', ''))
  ghb_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(!is.null(ghb$npghb) && ghb$npghb > 0){
    ghb$xyz_parm = ghb$condfact_parm = ghb$bhead_parm = ghb$column_parm = ghb$row_parm = ghb$layer_parm = list()
    
    i=1
    while(i <= ghb$npghb){
      # data set 3
      data_set_3 = rmfi_parse_variables(ghb_lines)
      ghb$parnam[i] =  as.character(data_set_3$variables[1])
      ghb$partyp[i] =  'GHB'
      ghb$parval[i] = as.numeric(data_set_3$variables[3])
      ghb$nlst[i] = as.numeric(data_set_3$variables[4])
      if(length(data_set_3$variables) > 4){
        ghb$instances[i]=T
        ghb$numinst[i] = as.numeric(data_set_3$variables[6])
      } 
      ghb_lines = data_set_3$remaining_lines
      rm(data_set_3)
      
      
      # time-varying parameters
      if(!is.null(ghb$instances) && ghb$instances[i]){
        ghb$layer_parm[[i]] = ghb$row_parm[[i]] = ghb$column_parm[[i]] = ghb$bhead_parm[[i]] = ghb$condfact_parm[[i]] = ghb$xyz_parm[[i]] = array(dim=c(ghb$numinst[i],ghb$nlst[i]))
        ghb$instnam[[i]] = vector(mode='character', length=ghb$numinst[i])
        
        j=1
        while(j <= ghb$numinst[i]){
          # data set 4a
          data_set_4a = rmfi_parse_variables(ghb_lines)
          ghb$instnam[[i]][j] =  as.character(data_set_4a$variables)
          ghb_lines = data_set_4a$remaining_lines
          rm(data_set_4a)
          
          k=1
          while(k <= ghb$nlst[i]){
            
            # data set 4b
            data_set_4b = rmfi_parse_variables(ghb_lines)
            ghb$layer_parm[[i]][j,k] = as.numeric(data_set_4b$variables[1])
            ghb$row_parm[[i]][j,k] = as.numeric(data_set_4b$variables[2])
            ghb$column_parm[[i]][j,k] = as.numeric(data_set_4b$variables[3])
            ghb$bhead_parm[[i]][j,k] = as.numeric(data_set_4b$variables[4])
            ghb$condfact_parm[[i]][j,k] = as.numeric(data_set_4b$variables[5])
            
            if(length(data_set_4b$variables) > 5) ghb$xyz_parm[[i]][j,k] = as.character(data_set_4b$variables[6])
            
            k=k+1
            ghb_lines = data_set_4b$remaining_lines
            rm(data_set_4b)
          }
          j = j+1
        } 
        
      } else {
        # non time-varying
        ghb$layer_parm[[i]] = ghb$row_parm[[i]] = ghb$column_parm[[i]] = ghb$bhead_parm[[i]] = ghb$condfact_parm[[i]] = ghb$xyz_parm[[i]] = array(dim=c(1, ghb$nlst[i]))
        
        k=1
        while(k <= ghb$nlst[i]){
          # data set 4b
          data_set_4b = rmfi_parse_variables(ghb_lines)
          ghb$layer_parm[[i]][1,k] = as.numeric(data_set_4b$variables[1])
          ghb$row_parm[[i]][1,k] = as.numeric(data_set_4b$variables[2])
          ghb$column_parm[[i]][1,k] = as.numeric(data_set_4b$variables[3])
          ghb$bhead_parm[[i]][1,k] = as.numeric(data_set_4b$variables[4])
          ghb$condfact_parm[[i]][1,k] = as.numeric(data_set_4b$variables[5])
          
          if(length(data_set_4b$variables) > 5) ghb$xyz_parm[[i]][1,k] = as.character(data_set_4b$variables[6])
          
          k=k+1
          ghb_lines = data_set_4b$remaining_lines
          rm(data_set_4b)
        }
        
      }
      
      i = i+1
    }
    if(all(is.na(unlist(ghb$xyz_parm)))) ghb$xyz_parm = NULL
  }
  
  # stress periods
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 = rmfi_parse_variables(ghb_lines)
    ghb$itmp[i] = as.numeric(data_set_5$variables[1])
    ghb$np[i] = as.numeric(data_set_5$variables[2])
    ghb_lines = data_set_5$remaining_lines
    rm(data_set_5)
    
    if(ghb$itmp[i] > 0){
      
      ghb$xyz_sp[[i]] = ghb$cond_sp[[i]] = ghb$bhead_sp[[i]] = ghb$column_sp[[i]] = ghb$row_sp[[i]] = ghb$layer_sp[[i]] = vector(length = ghb$itmp[i])
      for(j in 1:ghb$itmp[i]){
        # data set 6
        data_set_6 = rmfi_parse_variables(ghb_lines)
        ghb$layer_sp[[i]][j] = as.numeric(data_set_6$variables[1])
        ghb$row_sp[[i]][j] = as.numeric(data_set_6$variables[2])
        ghb$column_sp[[i]][j] = as.numeric(data_set_6$variables[3])
        ghb$bhead_sp[[i]][j] = as.numeric(data_set_6$variables[4])
        ghb$cond_sp[[i]][j] = as.numeric(data_set_6$variables[5])
        
        if(length(data_set_6$variables) > 5) ghb$xyz_sp[[i]][j] = as.character(data_set_6$variables[6])
        
        ghb_lines = data_set_6$remaining_lines
        rm(data_set_6)
      } 
    } 
    if(is.logical(unlist(ghb$xyz_sp)) && !any(unlist(ghb$xyz_sp))) ghb$xyz_sp = NULL
    
    if(ghb$np[i] > 0){
      ghb$iname[[i]] = ghb$pname[[i]] = vector(length=ghb$np[i])
      for(j in 1:ghb$np[i]){
        # data set 7
        data_set_7 = rmfi_parse_variables(ghb_lines)
        ghb$pname[[i]][j] = as.character(data_set_7$variables[1])
        if(length(data_set_7$variables) > 1) ghb$iname[[i]][j] = as.character(data_set_7$variables[2])
        
        ghb_lines = data_set_7$remaining_lines
        rm(data_set_7)
      }
    }
  }
  if(is.logical(unlist(ghb$iname)) && !any(unlist(ghb$iname))) ghb$iname = NULL
  
  class(ghb) = c('ghb', 'rmf_package')
  return(ghb)
  
}
