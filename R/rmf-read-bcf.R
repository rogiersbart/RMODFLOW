#' Read a MODFLOW bcf file
#' 
#' \code{rmf_read_bcf} reads in a MODFLOW block-centered flow file and returns it as an \code{RMODFLOW} bcf object
#' 
#' @param file filename; typically '*_bcf'
#' @param dis an \code{RMODFLOW} dis object
#' 
#' @return an \code{RMODFLOW} bcf object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_bcf}}, \code{\link{rmf_create_bcf}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}

rmf_read_bcf = function(file = {cat('Please select bcf file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}){
  
  bcf = list()
  bcf_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(bcf_lines)
  comment(bcf) = data_set_0$comments
  bcf_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(bcf_lines)
  bcf$ibcfcb = as.numeric(data_set_1$variables[1])
  bcf$hdry = as.numeric(data_set_1$variables[2])
  bcf$iwdflg = as.numeric(data_set_1$variables[3])
  bcf$wetfct = as.numeric(data_set_1$variables[4])
  bcf$iwetit = as.numeric(data_set_1$variables[5])
  bcf$ihdwet = as.numeric(data_set_1$variables[6])
  bcf_lines = data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  for(i in 1:dis$nlay){
    data_set_2 = rmfi_parse_variables(bcf_lines)
    bcf$int_trans[i] = as.numeric(data_set_2$variables[1])
    bcf$laycon[i] = as.numeric(data_set_2$variables[2])
    bcf_lines = data_set_2$remaining_lines
    rm(data_set_2)
  }
  
  # data set 3
  data_set_3 = rmfi_parse_variables(bcf_lines)
  bcf$trpy = as.numeric(data_set_3$variables)
  bcf_lines = data_set_3$remaining_lines
  rm(data_set_3)
  
  # data set 4-9
  bcf$wetdry = bcf$sf2 = bcf$hy = bcf$tran = bcf$sf1 = array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  bcf$vcont = array(dim=c(dis$nrow, dis$ncol, (dis$nlay-1)))
  
  for(i in 1:dis$nlay){
    
    # data set 4
    if('TR' %in% dis$sstr){
      data_set_4 = rmfi_parse_array(bcf_lines, nrow=dis$nrow, ncol = dis$ncol, nlay=1)
      bcf$sf1[,,i] = data_set_4$array
      bcf_lines = data_set_4$remaining_lines
      rm(data_set_4)
    }
    
    # data set 5
    if(bcf$laycon[i] %in% c(0,2)){
      data_set_5 = rmfi_parse_array(bcf_lines, nrow=dis$nrow, ncol=dis$ncol, nlay=1)
      bcf$tran[,,i] = data_set_5$array
      bcf_lines = data_set_5$remaining_lines
      rm(data_set_5)
    }
    
    # data set 6
    if(bcf$laycon[i] %in% c(1,3)){
      data_set_6 = rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1)
      bcf$hy[,,i] = data_set_6$array
      bcf_lines = data_set_6$remaining_lines
      rm(data_set_6)
    }
    
    # data set 7
    if(i != dis$nlay){
      data_set_7 = rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1)
      bcf$vcont[,,i] = data_set_7$array
      bcf_lines = data_set_7$remaining_lines
      rm(data_set_7)
    }
    
    # data set 8
    if(('TR' %in% dis$sstr) && bcf$laycon[i] %in% c(2,3)){
      data_set_8 = rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1)
      bcf$sf2[,,i] = data_set_8$array
      bcf_lines = data_set_8$remaining_lines
      rm(data_set_8)
    }
    
    # data set 9
    if((bcf$iwdflg != 0) && (bcf$laycon[i] %in% c(1,3))){
      data_set_9 = rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1)
      bcf$wetdry[,,i] = data_set_9$array
      bcf_lines = data_set_9$remaining_lines
      rm(data_set_9)
    } 
  }
  
  if(all(is.na(bcf$sf1))) bcf$sf1 = NULL
  if(all(is.na(bcf$tran))) bcf$tran = NULL
  if(all(is.na(bcf$hy))) bcf$hy = NULL
  if(all(is.na(bcf$sf2))) bcf$sf2 = NULL
  if(all(is.na(bcf$wetdry))) bcf$wetdry = NULL
  
  class(bcf) = c('bcf', 'rmf_package')
  return(bcf)
  
}