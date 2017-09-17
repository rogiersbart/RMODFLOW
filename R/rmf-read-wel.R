#' Read a MODFLOW well file
#' 
#' \code{rmf_read_wel} reads in a MODFLOW well file and returns it as an \code{RMODFLOW} wel object.
#'
#' @param file filename; typically '*.wel'
#' @param dis an \code{RMODFLOW} dis object
#' 
#' @return \code{RMODFLOW} wel object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_wel}}, \code{\link{rmf_create_wel}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?wel.htm}

rmf_read_wel = function(file = {cat('Please select well file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}
                        ){
  
  wel = list()
  wel_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(wel_lines)
  comment(wel) = data_set_0$comments
  wel_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(wel_lines)
  if('PARAMETER' %in% data_set_1$variables) {
    wel$npwel = as.numeric(data_set_1$variables[2])
    wel$mxl = as.numeric(data_set_1$variables[3])
    wel_lines = data_set_1$remaining_lines
  }  
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(wel_lines)
  wel$mxactw = as.numeric(data_set_2$variables[1])
  wel$iwelcb = as.numeric(data_set_2$variables[2])
  if(length(data_set_2$variables) > 2)   wel$option = c(as.character(data_set_2$variables[match(c('AUX', 'AUXILIARY'),data_set_2$variables)+1]), ifelse('NOPRINT' %in% data_set_2$variables, 'NOPRINT', ''))
  wel_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(!is.null(wel$npwel) && wel$npwel > 0){
    wel$xyz_parm = wel$qfact_parm = wel$column_parm = wel$row_parm = wel$layer_parm = list()
    
    i=1
    while(i <= wel$npwel){
      # data set 3
      data_set_3 = rmfi_parse_variables(wel_lines)
      wel$parnam[i] =  as.character(data_set_3$variables[1])
      wel$partyp[i] =  'Q'
      wel$parval[i] = as.numeric(data_set_3$variables[3])
      wel$nlst[i] = as.numeric(data_set_3$variables[4])
      if(length(data_set_3$variables) > 4){
        wel$instances[i]=T
        wel$numinst[i] = as.numeric(data_set_3$variables[6])
      } 
      wel_lines = data_set_3$remaining_lines
      rm(data_set_3)
      
      
      # time-varying parameters
      if(!is.null(wel$instances) && wel$instances[i]){
        wel$layer_parm[[i]] = wel$row_parm[[i]] = wel$column_parm[[i]] = wel$qfact_parm[[i]] = wel$xyz_parm[[i]] = array(dim=c(wel$numinst[i],wel$nlst[i]))
        wel$instnam[[i]] = vector(mode='character', length=wel$numinst[i])
        
        j=1
        while(j <= wel$numinst[i]){
          # data set 4a
          data_set_4a = rmfi_parse_variables(wel_lines)
          wel$instnam[[i]][j] =  as.character(data_set_4a$variables)
          wel_lines = data_set_4a$remaining_lines
          rm(data_set_4a)
          
          k=1
          while(k <= wel$nlst[i]){
            
            # data set 4b
            data_set_4b = rmfi_parse_variables(wel_lines)
            wel$layer_parm[[i]][j,k] = as.numeric(data_set_4b$variables[1])
            wel$row_parm[[i]][j,k] = as.numeric(data_set_4b$variables[2])
            wel$column_parm[[i]][j,k] = as.numeric(data_set_4b$variables[3])
            wel$qfact_parm[[i]][j,k] = as.numeric(data_set_4b$variables[4])
            if(length(data_set_4b$variables) > 4) wel$xyz_parm[[i]][j,k] = as.character(data_set_4b$variables[5])
           
            k=k+1
            wel_lines = data_set_4b$remaining_lines
            rm(data_set_4b)
          }
          j = j+1
        } 
        
      } else {
        # non time-varying
        wel$layer_parm[[i]] = wel$row_parm[[i]] = wel$column_parm[[i]] = wel$qfact_parm[[i]] = wel$xyz_parm[[i]] = array(dim=c(1, wel$nlst[i]))
        
        k=1
        while(k <= wel$nlst[i]){
          # data set 4b
          data_set_4b = rmfi_parse_variables(wel_lines)
          wel$layer_parm[[i]][1,k] = as.numeric(data_set_4b$variables[1])
          wel$row_parm[[i]][1,k] = as.numeric(data_set_4b$variables[2])
          wel$column_parm[[i]][1,k] = as.numeric(data_set_4b$variables[3])
          wel$qfact_parm[[i]][1,k] = as.numeric(data_set_4b$variables[4])
          if(length(data_set_4b$variables) > 4) wel$xyz_parm[[i]][1,k] = as.character(data_set_4b$variables[5])
          
          k=k+1
          wel_lines = data_set_4b$remaining_lines
          rm(data_set_4b)
        }
        
      }
      
      i = i+1
    }
    if(all(is.na(unlist(wel$xyz_parm)))) wel$xyz_parm = NULL
  }
  
  # stress periods
 
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 = rmfi_parse_variables(wel_lines)
    wel$itmp[i] = as.numeric(data_set_5$variables[1])
    wel$np[i] = as.numeric(data_set_5$variables[2])
    wel_lines = data_set_5$remaining_lines
    rm(data_set_5)
    
    if(wel$itmp[i] > 0){
      
      wel$xyz_sp[[i]] = wel$q_sp[[i]] = wel$column_sp[[i]] = wel$row_sp[[i]] = wel$layer_sp[[i]] = vector(length = wel$itmp[i])
      for(j in 1:wel$itmp[i]){
      # data set 6
      data_set_6 = rmfi_parse_variables(wel_lines)
      wel$layer_sp[[i]][j] = as.numeric(data_set_6$variables[1])
      wel$row_sp[[i]][j] = as.numeric(data_set_6$variables[2])
      wel$column_sp[[i]][j] = as.numeric(data_set_6$variables[3])
      wel$q_sp[[i]][j] = as.numeric(data_set_6$variables[4])
      if(length(data_set_6$variables) > 4) wel$xyz_sp[[i]][j] = as.character(data_set_6$variables[5])
      
      wel_lines = data_set_6$remaining_lines
      rm(data_set_6)
      } 
    } 
    if(is.logical(unlist(wel$xyz_sp)) && !any(unlist(wel$xyz_sp))) wel$xyz_sp = NULL
    
    if(wel$np[i] > 0){
      wel$iname[[i]] = wel$pname[[i]] = vector(length=wel$np[i])
      for(j in 1:wel$np[i]){
        # data set 7
        data_set_7 = rmfi_parse_variables(wel_lines)
        wel$pname[[i]][j] = as.character(data_set_7$variables[1])
        if(length(data_set_7$variables) > 1) wel$iname[[i]][j] = as.character(data_set_7$variables[2])
        
        wel_lines = data_set_7$remaining_lines
        rm(data_set_7)
      }
    }
  }
  if(is.logical(unlist(wel$iname)) && !any(unlist(wel$iname))) wel$iname = NULL
  
  class(wel) = c('wel', 'rmf_package')
  return(wel)
  
}
