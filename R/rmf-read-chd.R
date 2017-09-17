#' Read a MODFLOW time-variant specified-head file
#' 
#' \code{rmf_read_chd} reads in a MODFLOW time-variant specified-head file and returns it as an \code{RMODFLOW} chd object.
#'
#' @param file filename; typically '*.chd'
#' @param dis an \code{RMODFLOW} dis object
#' 
#' @return \code{RMODFLOW} chd object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_chd}}, \code{\link{rmf_create_chd}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?chd.htm}

rmf_read_chd = function(file = {cat('Please select time-variant specified-head file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}
){
  
  chd = list()
  chd_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(chd_lines)
  comment(chd) = data_set_0$comments
  chd_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(chd_lines)
  if('PARAMETER' %in% data_set_1$variables) {
    chd$npchd = as.numeric(data_set_1$variables[2])
    chd$mxl = as.numeric(data_set_1$variables[3])
    chd_lines = data_set_1$remaining_lines
  }  
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(chd_lines)
  chd$mxactc = as.numeric(data_set_2$variables[1])
  if(length(data_set_2$variables) > 1)   chd$option = c(as.character(data_set_2$variables[match(c('AUX', 'AUXILIARY'),data_set_2$variables)+1]), ifelse('NOPRINT' %in% data_set_2$variables, 'NOPRINT', ''))
  chd_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(!is.null(chd$npchd) && chd$npchd > 0){
    chd$xyz_parm = chd$ehdfact_parm = chd$shdfact_parm = chd$column_parm = chd$row_parm = chd$layer_parm = list()
    
    i=1
    while(i <= chd$npchd){
      # data set 3
      data_set_3 = rmfi_parse_variables(chd_lines)
      chd$parnam[i] =  as.character(data_set_3$variables[1])
      chd$partyp[i] =  'CHD'
      chd$parval[i] = as.numeric(data_set_3$variables[3])
      chd$nlst[i] = as.numeric(data_set_3$variables[4])
      if(length(data_set_3$variables) > 4){
        chd$instances[i]=T
        chd$numinst[i] = as.numeric(data_set_3$variables[6])
      } 
      chd_lines = data_set_3$remaining_lines
      rm(data_set_3)
      
      
      # time-varying parameters
      if(!is.null(chd$instances) && chd$instances[i]){
        chd$layer_parm[[i]] = chd$row_parm[[i]] = chd$column_parm[[i]] = chd$shdfact_parm[[i]] = chd$ehdfact_parm[[i]] = chd$xyz_parm[[i]] = array(dim=c(chd$numinst[i],chd$nlst[i]))
        chd$instnam[[i]] = vector(mode='character', length=chd$numinst[i])
        
        j=1
        while(j <= chd$numinst[i]){
          # data set 4a
          data_set_4a = rmfi_parse_variables(chd_lines)
          chd$instnam[[i]][j] =  as.character(data_set_4a$variables)
          chd_lines = data_set_4a$remaining_lines
          rm(data_set_4a)
          
          k=1
          while(k <= chd$nlst[i]){
            
            # data set 4b
            data_set_4b = rmfi_parse_variables(chd_lines)
            chd$layer_parm[[i]][j,k] = as.numeric(data_set_4b$variables[1])
            chd$row_parm[[i]][j,k] = as.numeric(data_set_4b$variables[2])
            chd$column_parm[[i]][j,k] = as.numeric(data_set_4b$variables[3])
            chd$shdfact_parm[[i]][j,k] = as.numeric(data_set_4b$variables[4])
            chd$ehdfact_parm[[i]][j,k] = as.numeric(data_set_4b$variables[5])

            if(length(data_set_4b$variables) > 5) chd$xyz_parm[[i]][j,k] = as.character(data_set_4b$variables[6])
            
            k=k+1
            chd_lines = data_set_4b$remaining_lines
            rm(data_set_4b)
          }
          j = j+1
        } 
        
      } else {
        # non time-varying
        chd$layer_parm[[i]] = chd$row_parm[[i]] = chd$column_parm[[i]] = chd$shdfact_parm[[i]] = chd$ehdfact_parm[[i]] = chd$xyz_parm[[i]] = array(dim=c(1, chd$nlst[i]))
        
        k=1
        while(k <= chd$nlst[i]){
          # data set 4b
          data_set_4b = rmfi_parse_variables(chd_lines)
          chd$layer_parm[[i]][1,k] = as.numeric(data_set_4b$variables[1])
          chd$row_parm[[i]][1,k] = as.numeric(data_set_4b$variables[2])
          chd$column_parm[[i]][1,k] = as.numeric(data_set_4b$variables[3])
          chd$shdfact_parm[[i]][1,k] = as.numeric(data_set_4b$variables[4])
          chd$ehdfact_parm[[i]][1,k] = as.numeric(data_set_4b$variables[5])

          if(length(data_set_4b$variables) > 5) chd$xyz_parm[[i]][1,k] = as.character(data_set_4b$variables[6])
          
          k=k+1
          chd_lines = data_set_4b$remaining_lines
          rm(data_set_4b)
        }
        
      }
      
      i = i+1
    }
    if(all(is.na(unlist(chd$xyz_parm)))) chd$xyz_parm = NULL
  }
  
  # stress periods
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 = rmfi_parse_variables(chd_lines)
    chd$itmp[i] = as.numeric(data_set_5$variables[1])
    chd$np[i] = as.numeric(data_set_5$variables[2])
    chd_lines = data_set_5$remaining_lines
    rm(data_set_5)
    
    if(chd$itmp[i] > 0){
      
      chd$xyz_sp[[i]] = chd$ehead_sp[[i]] = chd$shead_sp[[i]] = chd$column_sp[[i]] = chd$row_sp[[i]] = chd$layer_sp[[i]] = vector(length = chd$itmp[i])
      for(j in 1:chd$itmp[i]){
        # data set 6
        data_set_6 = rmfi_parse_variables(chd_lines)
        chd$layer_sp[[i]][j] = as.numeric(data_set_6$variables[1])
        chd$row_sp[[i]][j] = as.numeric(data_set_6$variables[2])
        chd$column_sp[[i]][j] = as.numeric(data_set_6$variables[3])
        chd$shead_sp[[i]][j] = as.numeric(data_set_6$variables[4])
        chd$ehead_sp[[i]][j] = as.numeric(data_set_6$variables[5])

        if(length(data_set_6$variables) > 5) chd$xyz_sp[[i]][j] = as.character(data_set_6$variables[6])
        
        chd_lines = data_set_6$remaining_lines
        rm(data_set_6)
      } 
    } 
    if(is.logical(unlist(chd$xyz_sp)) && !any(unlist(chd$xyz_sp))) chd$xyz_sp = NULL
    
    if(chd$np[i] > 0){
      chd$iname[[i]] = chd$pname[[i]] = vector(length=chd$np[i])
      for(j in 1:chd$np[i]){
        # data set 7
        data_set_7 = rmfi_parse_variables(chd_lines)
        chd$pname[[i]][j] = as.character(data_set_7$variables[1])
        if(length(data_set_7$variables) > 1) chd$iname[[i]][j] = as.character(data_set_7$variables[2])
        
        chd_lines = data_set_7$remaining_lines
        rm(data_set_7)
      }
    }
  }
  if(is.logical(unlist(chd$iname)) && !any(unlist(chd$iname))) chd$iname = NULL
  
  class(chd) = c('chd', 'rmf_package')
  return(chd)
  
}
