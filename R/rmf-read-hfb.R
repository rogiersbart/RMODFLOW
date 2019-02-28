#' Read a MODFLOW horizontal flow barrier file
#' 
#' \code{rmf_read_hfb} reads in a MODFLOW horizontal flow barrier file and returns it as an \code{RMODFLOW} hfb object.
#'
#' @param file filename; typically '*.hfb'
#' 
#' @return \code{RMODFLOW} hfb object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_hfb}}, \code{\link{rmf_create_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}

rmf_read_hfb = function(file = {cat('Please select horizontal flow barrier file ...\n'); file.choose()}
){
  
  hfb = list()
  hfb_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(hfb_lines)
  comment(hfb) = data_set_0$comments
  hfb_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(hfb_lines)
  hfb$nphfb = as.numeric(data_set_1$variables[1])
  hfb$mxfb = as.numeric(data_set_1$variables[2])
  hfb$nhfbnp = as.numeric(data_set_1$variables[3])
  if(length(data_set_1$variables) > 3) hfb$option = 'NOPRINT'
  hfb_lines = data_set_1$remaining_lines
  rm(data_set_1)
  
  # parameters
  if(hfb$nphfb > 0){
    
    hfb$factor_parm = hfb$icol2_parm = hfb$irow2_parm = hfb$icol1_parm = hfb$irow1_parm = hfb$layer_parm = list()
    
    i=1
    while(i <= hfb$nphfb){
      # data set 2
      data_set_2 = rmfi_parse_variables(hfb_lines)
      hfb$parnam[i] =  as.character(data_set_2$variables[1])
      hfb$partyp[i] = 'HFB'
      hfb$parval[i] = as.numeric(data_set_2$variables[3])
      hfb$nlst[i] = as.numeric(data_set_2$variables[4])
      hfb_lines = data_set_2$remaining_lines
      rm(data_set_2)
      
      
      hfb$factor_parm[[i]] = hfb$icol2_parm[[i]] = hfb$irow2_parm[[i]] = hfb$icol1_parm[[i]] = hfb$irow1_parm[[i]] = hfb$layer_parm[[i]] = list()
      
      j=1
      while(j <= hfb$nlst[i]){
        # data set 3
        data_set_3 = rmfi_parse_variables(hfb_lines)
        hfb$layer_parm[[i]][j] = as.numeric(data_set_3$variables[1])
        hfb$irow1_parm[[i]][j] = as.numeric(data_set_3$variables[2])
        hfb$icol1_parm[[i]][j] = as.numeric(data_set_3$variables[3])
        hfb$irow2_parm[[i]][j] = as.numeric(data_set_3$variables[4])
        hfb$icol2_parm[[i]][j] = as.numeric(data_set_3$variables[5])
        hfb$factor_parm[[i]][j] = as.numeric(data_set_3$variables[6])
        hfb_lines = data_set_3$remaining_lines
        rm(data_set_3)
        j = j+1
      }
      i = i+1
    }
  }
  
  # data set 4
  if(hfb$nhfbnp > 0){
    for(i in 1:hfb$nhfbnp){
      data_set_4 = rmfi_parse_variables(hfb_lines)
      hfb$layer_noparm[i] = as.numeric(data_set_4$variables[1])
      hfb$irow1_noparm[i] = as.numeric(data_set_4$variables[2])
      hfb$icol1_noparm[i] = as.numeric(data_set_4$variables[3])
      hfb$irow1_noparm[i] = as.numeric(data_set_4$variables[4])
      hfb$icol2_noparm[i] = as.numeric(data_set_4$variables[5])
      hfb$hydchr_noparm[i] = as.numeric(data_set_4$variables[6])
      hfb_lines = data_set_4$remaining_lines
      rm(data_set_4)
    }
  }
  
  # data set 5
  data_set_5 = rmfi_parse_variables(hfb_lines)
  hfb$nacthfb = as.numeric(data_set_5$variables[1])
  hfb_lines = data_set_5$remaining_lines
  rm(data_set_5)
  
  # data set 6
  if(hfb$nacthfb > 0){
    for(i in 1:hfb$nacthfb){
      data_set_6 = rmfi_parse_variables(hfb_lines)
      hfb$pname[i] = as.character(data_set_6$variables[1])
      hfb_lines = data_set_6$remaining_lines
      rm(data_set_6)
    }
  }
  
  class(hfb) = c('hfb', 'rmf_package')
  return(hfb)
  
}
