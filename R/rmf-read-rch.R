#' Read a MODFLOW recharge file
#' 
#' \code{rmf_read_rch} reads in a MODFLOW recharge file and returns it as an \code{RMODFLOW} rch object.
#'
#' @param file filename; typically '*.rch'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return \code{RMODFLOW} rch object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_rch}}, \code{\link{rmf_create_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}

rmf_read_rch = function(file = {cat('Please select rch file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                        ... ){
  
  rch = list()
  rch_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(rch_lines)
  comment(rch) = data_set_0$comments
  rch_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(rch_lines)
  if('PARAMETER' %in% data_set_1$variables) {
    rch$nprch = as.numeric(data_set_1$variables[2])
    rch_lines = data_set_1$remaining_lines
  }  
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(rch_lines)
  rch$nrchop = as.numeric(data_set_2$variables[1])
  rch$irchcb = as.numeric(data_set_2$variables[2])
  rch_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(!is.null(rch$nprch) && rch$nprch > 0){
    
    rch$iz = rch$zonarr = rch$mltarr = list()
    
    i=1
    while(i <= rch$nprch){
      # data set 3
      data_set_3 = rmfi_parse_variables(rch_lines)
      rch$parnam[i] =  as.character(data_set_3$variables[1])
      rch$partyp[i] = 'RCH'
      rch$parval[i] = as.numeric(data_set_3$variables[3])
      rch$nclu[i] = as.numeric(data_set_3$variables[4])
      if(length(data_set_3$variables) > 4){
        rch$instances[i]=T
        rch$numinst[i] = as.numeric(data_set_3$variables[6])
      } 
      rch_lines = data_set_3$remaining_lines
      rm(data_set_3)
      
      
      
      # time-varying parameters
      if(!is.null(rch$instances) && rch$instances[i]){
        rch$iz[[i]] = rch$zonarr[[i]] = rch$mltarr[[i]] = array(dim=c(rch$numinst[i],rch$nclu[i]))
        rch$instnam[[i]] = vector(mode='character', length=rch$numinst[i])
        
        j=1
        while(j <= rch$numinst[i]){
          # data set 4a
          data_set_4a = rmfi_parse_variables(rch_lines)
          rch$instnam[[i]][j] =  as.character(data_set_4a$variables)
          rch_lines = data_set_4a$remaining_lines
          rm(data_set_4a)
          
          k=1
          while(k <= rch$nclu[i]){
            
            # data set 4b
            data_set_4b = rmfi_parse_variables(rch_lines)
            rch$mltarr[[i]][j,k] = as.character(data_set_4b$variables[1])
            rch$zonarr[[i]][j,k] = as.character(data_set_4b$variables[2])
            if(length(data_set_4b$variables) > 2) rch$iz[[i]][j,k] = paste(data_set_4b$variables[-c(1:2)], collapse=' ')

            k=k+1
            rch_lines = data_set_4b$remaining_lines
            rm(data_set_4b)
          }
          j = j+1
        } 
        
      } else {
        # non time-varying
        rch$iz[[i]] = rch$zonarr[[i]] = rch$mltarr[[i]] = array(dim=c(1,rch$nclu[i]))
        
        k=1
        while(k <= rch$nclu[i]){
          # data set 4b
          
          data_set_4b = rmfi_parse_variables(rch_lines)
          rch$mltarr[[i]][1,k] = as.character(data_set_4b$variables[1])
          rch$zonarr[[i]][1,k] = as.character(data_set_4b$variables[2])
          if(length(data_set_4b$variables) > 2) rch$iz[[i]][1,k] = paste(data_set_4b$variables[-c(1:2)], collapse=' ')

          
          k=k+1
          rch_lines = data_set_4b$remaining_lines
          rm(data_set_4b)
        }
        
      }
      
      i = i+1
    }
    if(all(is.na(unlist(rch$iz)))) rch$iz = NULL
  }
  
  # stress periods
  
  if(((!is.null(rch$nprch) && rch$nprch==0) || is.null(rch$nprch))) rch$rech = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nper))
  if(!is.null(rch$nprch) && rch$nprch > 0){
    rch$pname = list()
    if(!is.null(rch$instances) && T %in% rch$instances) rch$iname = list()
    rch$irchpf = list()
  }
  if(rch$nrchop==2){
    rch$inirch = vector(mode='numeric', length=dis$nper)
    rch$irch = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nper))
  }
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 = rmfi_parse_variables(rch_lines)
    rch$inrech[i] = as.numeric(data_set_5$variables[1])
    if(length(data_set_5$variables) > 1) rch$inirch[i] = as.numeric(data_set_5$variables[2])
    rch_lines = data_set_5$remaining_lines
    rm(data_set_5)
    
    # data set 6
    if(((!is.null(rch$nprch) && rch$nprch==0) || is.null(rch$nprch)) && rch$inrech[i] >= 0) {
      data_set_6 = rmfi_parse_array(rch_lines, nrow = dis$nrow, ncol=dis$ncol, nlay=1, file = file, ...)
      rch$rech[,,i] = data_set_6$array
      rch_lines = data_set_6$remaining_lines
      rm(data_set_6)
    }
    
    if((!is.null(rch$nprch) && rch$nprch[i] > 0) && rch$inrech[i] > 0){
      rch$iname[[i]] = rch$pname[[i]] = vector(length=rch$inrech[i])
      rch$irchpf[[i]] = vector(mode='numeric', length=rch$inrech[i])
      
      for(j in 1:rch$inrech[i]){
        # data set 7
        data_set_7 = rmfi_parse_variables(rch_lines)
        rch$pname[[i]][j] = as.character(data_set_7$variables[1])
        if((length(data_set_7$variables) > 1) && (!is.null(rch$instances) && rch$instances[which(rch$parnam == rch$pname[[i]][j])])){
          rch$iname[[i]][j] = as.character(data_set_7$variables[2])
        } 
        if((length(data_set_7$variables) > 1) && (is.null(rch$instances) || (!is.null(rch$instances) && !rch$instances[which(rch$parnam == rch$pname[[i]][j])]))){
          rch$irchpf[[i]][j] = as.numeric(data_set_7$variables[2])
        } 
        rch_lines = data_set_7$remaining_lines
        rm(data_set_7)
      }
    }
    if(is.logical(unlist(rch$iname)) && !any(unlist(rch$iname))) rch$iname = NULL
    if(is.logical(unlist(rch$irchpf)) && !any(unlist(rch$irchpf))) rch$irchpf = NULL
    
    
    if(rch$nrchop == 2 && (!is.null(rch$inirch) && rch$inirch[i] >= 0)){
      # data set 8
      data_set_8 = rmfi_parse_array(rch_lines, nrow=dis$nrow, ncol=dis$ncol, nlay=1, file = file, ...)
      rch$irch[,,i] = data_set_8$array
      rch_lines = data_set_8$remaining_lines
      rm(data_set_8)
    } 
    
  }
  
  class(rch) = c('rch', 'rmf_package')
  return(rch)
  
}