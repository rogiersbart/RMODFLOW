#' Create an \code{RMODFLOW} rch object.
#' 
#' \code{rmf_create_rch} creates an \code{RMODFLOW} rch object
#' 
#' @param nprch number of rch parameters; defaults to NULL
#' @param nrchop recharge option code; defaults to 1
#' @param irchcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param parnam vector of length \code{nprch} specifying the parameter names; defaults to NULL
#' @param parval vector of length \code{nprch} specifying the parameter values; defaults to NULL
#' @param nclu vector of length \code{nprch} specifying the number of clusters that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to NULL
#' @param instances logical vector of length \code{nprch} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{nprch} indicating the number of instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{nprch} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param mltarr list with \code{nprch} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} specifying the multiplier array name of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param zonarr list with \code{nprch} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} specifying the zone array name of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param iz list with \code{nprch} elements where each element \code{i} is a numeric 2D array with dimensions \code{numinst x nclu} with each element a single zone number or zone numbers separated by spaces for parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param inrech numeric vector of length \code{dis$nper} specifying the \code{rech} read flag which depends on whether or not parameters are specified; defaults to 1
#' @param inirch numeric vector of length \code{dis$nper} specifying the \code{irch} read flag which depends on whether or not parameters are specified; defaults to NULL
#' @param rech numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the recharge fluxes; defaults to 1e-8 for all cells in the top layer
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{inrech} for stress period \code{i} specifying the names of the parameters being used; defaults to NULL
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{inrech} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' @param irchpf optional list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{inrech} for stress period \code{i} specifying the format code for printing \code{rech} after it has been defined by parameters, defaults to NULL 
#' @param irch numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the layer numbers defining in which layer recharge is applied; defaults to NULL
#' 
#' @return \code{RMODFLOW} rch object
#' @export
#' @seealso \code{\link{rmf_read_rch}}, \code{\link{rmf_write_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}


rmf_create_rch = function(nprch = NULL,
                          nrchop = 1,
                          irchcb = 0,
                          parnam = NULL, 
                          parval = NULL, 
                          nclu = NULL, 
                          instances = NULL,
                          numinst = NULL,
                          instnam = NULL,
                          mltarr = NULL, 
                          zonarr = NULL,
                          iz = NULL,
                          inrech = 1,
                          inirch = NULL,
                          rech = array(1e-8, dim=c(10, 10, 1)), 
                          pname = NULL, 
                          iname = NULL,
                          irchpf = NULL,
                          irch = NULL
){
  
  rch = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting rch object
  
  # data set 1
  if(!is.null(nprch)) rch$nprch = nprch
  
  # data set 2
  rch$nrchop = nrchop
  rch$irchcb = irchcb
  
  
  if(!is.null(rch$nprch) && rch$nprch > 0){
    
    # data set 3
    rch$parnam = parnam
    rch$partyp = rep('RCH', rch$nprch)
    rch$parval = parval
    rch$nclu = nclu
    if(!is.null(instances) && T %in% instances) rch$instances = instances
    if(!is.null(rch$instances)) rch$numinst = numinst      
    
    
    # data set 4a
    if(!is.null(rch$instances) && T %in% rch$instances) rch$instnam = instnam
    
    # data set 4b
    rch$mltarr = mltarr
    rch$zonarr = zonarr
    rch$iz = iz
    
  }
  
  
  # data set 5
  rch$inrech = inrech
  if(rch$nrchop==2) rch$inirch = inirch
  
  # data set 6
  if ((is.null(rch$nprch) || (!is.null(rch$nprch) && rch$nprch == 0)) && any(rch$inrech >= 0)) rch$rech = rech
  
  # data set 7
  if ((!is.null(rch$nprch) && rch$nprch > 0) && any(rch$inrech > 0)){
    rch$pname = pname
    if (!is.null(rch$instances) && T %in% rch$instances) rch$iname = iname
    if (!is.null(irchpf) && (is.null(rch$instances) || (!is.null(rch$instances) && !(T %in% rch$instances))) ) rch$irchpf = irchpf
  } 
  
  # data set 8
  if(rch$nrchop == 2 && any(rch$inirch >= 0)) rch$irch = irch
  
  class(rch) = c('rch', 'rmf_package')
  return(rch)
  
}

#' Read a MODFLOW recharge file
#' 
#' \code{rmf_read_rch} reads in a MODFLOW recharge file and returns it as an \code{RMODFLOW} rch object.
#'
#' @param file filename; typically '*.rch'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return \code{RMODFLOW} rch object
#' @export
#' @seealso \code{\link{rmf_write_rch}}, \code{\link{rmf_create_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}

rmf_read_rch = function(file = {cat('Please select rch file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                        ... ){
  
  rch = list()
  rch_lines = readr::read_lines(file)
  
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

#' Write a MODFLOW recharge file
#'
#' \code{rmf_write_rch} writes a MODFLOW recharge file based on an \code{RMODFLOW} rch object
#' 
#' @param rch an \code{RMODFLOW} rch object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.rch'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_rch}}, \code{\link{rmf_create_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}


rmf_write_rch = function(rch, dis=rmf_read_dis(), file = {cat('Please select rch file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Recharge Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(rch)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(rch$nprch) && rch$nprch > 0 ) rmfi_write_variables('PARAMETER', rch$nprch, file=file)
  
  # data set 2
  rmfi_write_variables(rch$nrchop, rch$irchcb, file=file)
  
  # parameters
  if(!is.null(rch$nprch) && rch$nprch > 0){
    for (i in 1:rch$nprch){
      # data set 3
      rmfi_write_variables(rch$parnam[i], rch$partyp[i], rch$parval[i], rch$nclu[i], ifelse(rch$instances[i], 'INSTANCES', ' '), ifelse(rch$instances[i], rch$numinst[i], ' '), file=file)
      
      # time-varying
      if(!is.null(rch$instances) && rch$instances[i]){
        for (j in 1:rch$numinst[i]){
          # data set 4a
          if(rch$instances[i]) rmfi_write_variables(rch$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:rch$nclu[i]){
            rmfi_write_variables(rch$mltarr[[i]][j, k], rch$zonarr[[i]][j,k], ifelse(!is.null(rch$iz) && rch$zonarr[[i]][j,k]!='ALL', rch$iz[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        # data set 4b
        for (k in 1:rch$nclu[i]){
          rmfi_write_variables(rch$mltarr[[i]][1, k], rch$zonarr[[i]][1,k], ifelse(!is.null(rch$iz) && rch$zonarr[[i]][1,k]!='ALL', rch$iz[[i]][1,k], ''), file=file)
          
        }
      }
      
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    rmfi_write_variables(rch$inrech[i], rch$inirch[i], file=file)
    
    # data set 6
    if(((!is.null(rch$nprch) && rch$nprch==0) || is.null(rch$nprch)) && rch$inrech[i] >= 0) rmfi_write_array(rch$rech[,,i], file=file)
    
    # data set 7
    if((!is.null(rch$nprch) && rch$nprch > 0) && rch$inrech[i] > 0){
      for (j in 1:rch$inrech[i]){
        
        rmfi_write_variables(rch$pname[[i]][j], ifelse(!is.null(rch$instances) && rch$instances[which(rch$parnam==rch$pname[[i]][j])], rch$iname[[i]][j], ''), ifelse((is.null(rch$instances) || (!is.null(rch$instances) && !(rch$instances[which(rch$parnam==rch$pname[[i]][j])]))) && !is.null(rch$irchpf), rch$irchpf[[i]][j], ' '), file=file) 
        
      }
    }
    
    # data set 8
    if(rch$nrchop==2 && (!is.null(rch$inirch) && rch$inirch[i] >= 0)){
      rmfi_write_array(rch$irch[,,i], file=file)
    }
    
  }
  
  
}