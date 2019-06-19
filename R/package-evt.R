#' Create an \code{RMODFLOW} evt object.
#' 
#' \code{rmf_create_evt} creates an \code{RMODFLOW} evt object
#' ' 
#' @param npevt number of evt parameters; defaults to NULL
#' @param nevtop evapotranspiration option code; defaults to 1
#' @param ievtcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param parnam vector of length \code{npevt} specifying the parameter names; defaults to NULL
#' @param parval vector of length \code{npevt} specifying the parameter values; defaults to NULL
#' @param nclu vector of length \code{npevt} specifying the number of clusters that are included in a non-time-varying parameter or in each instance of a time-varying parameter; defaults to NULL
#' @param instances logical vector of length \code{npevt} indicating which parameters are time-varying; defaults to NULL
#' @param numinst vector of length \code{npevt} indicating the number of instances that are included in the time-varying parameter; defaults to NULL
#' @param instnam list with \code{npevt} elements where each element \code{i} is a character vector of length \code{numinst} for parameter \code{i} specifying the names of the parameter instances. If not time-varying, set numinst dimension to 1. Defaults to NULL
#' @param mltarr list with \code{npevt} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} specifying the multiplier array name of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param zonarr list with \code{npevt} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} specifying the zone array name of parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param iz list with \code{npevt} elements where each element \code{i} is a character 2D array with dimensions \code{numinst x nclu} with each element a single zone number or zone numbers separated by spaces for parameter \code{i}. If the parameter is not time varying, set the numinst dimension to 1. Defaults to NULL
#' @param insurf numeric vector of length \code{dis$nper} specifying the \code{surf} read flag; defaults to 1
#' @param inevtr numeric vector of length \code{dis$nper} specifying the \code{evtr} read flag which depends on whether or not parameters are specified; defaults to 1
#' @param inexdp numeric vector of length \code{dis$nper} specifying the \code{exdp} read flag; defaults to 1
#' @param inievt numeric vector of length \code{dis$nper} specifying the \code{ievt} read flag; defaults to NULL
#' @param surf numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the elevation of the ET surface; defaults to the 0 for all cells
#' @param evtr numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the maximum ET flux; defaults to 4e-9 for all cells
#' @param pname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{inevtr} for stress period \code{i} specifying the names of the parameters being used; defaults to NULL
#' @param iname list with \code{dis$nper} elements where each element \code{i} is a character vector of length \code{inevtr} for stress period \code{i} specifying the names of the parameter instances being used; defaults to NULL
#' @param ievtpf optional list with \code{dis$nper} elements where each element \code{i} is a numeric vector of length \code{inevt} for stress period \code{i} specifying the format code for printing \code{evtr} after it has been defined by parameters, defaults to NULL 
#' @param exdp numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the ET extinction depths as distances from \code{surf}; defaults to 2 for all cells
#' @param ievt numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nper} specifying the layer numbers defining in which layer ET is removed; defaults to NULL
#' 
#' @return \code{RMODFLOW} evt object
#' @export
#' @seealso \code{\link{rmf_read_evt}}, \code{\link{rmf_write_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}


rmf_create_evt = function(npevt = NULL,
                          nevtop = 1,
                          ievtcb = 0,
                          parnam = NULL, 
                          parval = NULL, 
                          nclu = NULL, 
                          instances = NULL,
                          numinst = NULL,
                          instnam = NULL,
                          mltarr = NULL, 
                          zonarr = NULL,
                          iz = NULL,
                          insurf = 1,
                          inevtr = 1,
                          inexdp = 1,
                          inievt = NULL,
                          pname = NULL, 
                          iname = NULL,
                          ievtpf = NULL,
                          surf = array(0, dim=c(10, 10, 1)), 
                          evtr = array(4e-9, dim=c(10, 10, 1)), 
                          exdp = array(2, dim=c(10, 10, 1)),
                          ievt = NULL
){
  
  evt = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting evt object
  
  # data set 1
  if(!is.null(npevt)) evt$npevt = npevt
  
  # data set 2
  evt$nevtop = nevtop
  evt$ievtcb = ievtcb
  
  
  if(!is.null(evt$npevt) && evt$npevt > 0){
    
    # data set 3
    evt$parnam = parnam
    evt$partyp = rep('EVT', evt$npevt)
    evt$parval = parval
    evt$nclu = nclu
    if(!is.null(instances) && T %in% instances) evt$instances = instances
    if(!is.null(evt$instances)) evt$numinst = numinst      
    
    
    # data set 4a
    if(!is.null(evt$instances) && T %in% evt$instances) evt$instnam = instnam
    
    # data set 4b
    evt$mltarr = mltarr
    evt$zonarr = zonarr
    evt$iz = iz
    
  }
  
  
  # data set 5
  evt$insurf = insurf
  evt$inevtr = inevtr
  evt$inexdp = inexdp
  if(evt$nevtop==2) evt$inievt = inievt
  
  # data set 6
  if(any(evt$insurf >= 0)) evt$surf = surf
  
  # data set 7
  if ((is.null(evt$npevt) || (!is.null(evt$npevt) && evt$npevt == 0)) && any(evt$inevtr >= 0)) evt$evtr = evtr
  
  # data set 8
  if ((!is.null(evt$npevt) && evt$npevt > 0) && any(evt$inevtr > 0)){
    evt$pname = pname
    if (!is.null(evt$instances) && T %in% evt$instances) evt$iname = iname
    if (!is.null(ievtpf) && (is.null(evt$instances) || (!is.null(evt$instances) && !(T %in% evt$instances))) ) evt$ievtpf = ievtpf
  } 
  
  # data set 9
  if(any(evt$inexdp >= 0)) evt$exdp = exdp
  
  # data set 10
  if(evt$nevtop == 2 && any(evt$inievt >= 0)) evt$ievt = ievt
  
  class(evt) = c('evt', 'rmf_package')
  return(evt)
  
}

#' Read a MODFLOW evapotranspiration file
#' 
#' \code{rmf_read_evt} reads in a MODFLOW evapotranspiration file and returns it as an \code{RMODFLOW} evt object.
#'
#' @param file filename; typically '*.evt'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return \code{RMODFLOW} evt object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_evt}}, \code{\link{rmf_create_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}

rmf_read_evt = function(file = {cat('Please select evapotranspiration file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                        ...) {
  
  evt = list()
  evt_lines = read_lines(file)
  
  # data set 0
  data_set_0 = rmfi_parse_comments(evt_lines)
  comment(evt) = data_set_0$comments
  evt_lines = data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(evt_lines)
  if('PARAMETER' %in% data_set_1$variables) {
    evt$npevt = as.numeric(data_set_1$variables[2])
    evt_lines = data_set_1$remaining_lines
  }  
  rm(data_set_1)
  
  # data set 2
  data_set_2 = rmfi_parse_variables(evt_lines)
  evt$nevtop = as.numeric(data_set_2$variables[1])
  evt$ievtcb = as.numeric(data_set_2$variables[2])
  evt_lines = data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(!is.null(evt$npevt) && evt$npevt > 0){
    
    evt$iz = evt$zonarr = evt$mltarr = list()
    
    i=1
    while(i <= evt$npevt){
      # data set 3
      data_set_3 = rmfi_parse_variables(evt_lines)
      evt$parnam[i] =  as.character(data_set_3$variables[1])
      evt$partyp[i] = 'EVT'
      evt$parval[i] = as.numeric(data_set_3$variables[3])
      evt$nclu[i] = as.numeric(data_set_3$variables[4])
      if(length(data_set_3$variables) > 4){
        evt$instances[i]=T
        evt$numinst[i] = as.numeric(data_set_3$variables[6])
      } 
      evt_lines = data_set_3$remaining_lines
      rm(data_set_3)
      
      
      
      # time-varying parameters
      if(!is.null(evt$instances) && evt$instances[i]){
        evt$iz[[i]] = evt$zonarr[[i]] = evt$mltarr[[i]] = array(dim=c(evt$numinst[i],evt$nclu[i]))
        evt$instnam[[i]] = vector(mode='character', length=evt$numinst[i])
        
        j=1
        while(j <= evt$numinst[i]){
          # data set 4a
          data_set_4a = rmfi_parse_variables(evt_lines)
          evt$instnam[[i]][j] =  as.character(data_set_4a$variables)
          evt_lines = data_set_4a$remaining_lines
          rm(data_set_4a)
          
          k=1
          while(k <= evt$nclu[i]){
            
            # data set 4b
            data_set_4b = rmfi_parse_variables(evt_lines)
            evt$mltarr[[i]][j,k] = as.character(data_set_4b$variables[1])
            evt$zonarr[[i]][j,k] = as.character(data_set_4b$variables[2])
            if(length(data_set_4b$variables) > 2) evt$iz[[i]][j,k] = paste(data_set_4b$variables[-c(1:2)], collapse=' ')
            
            
            k=k+1
            evt_lines = data_set_4b$remaining_lines
            rm(data_set_4b)
          }
          j = j+1
        } 
        
      } else {
        # non time-varying
        evt$iz[[i]] = evt$zonarr[[i]] = evt$mltarr[[i]] = array(dim=c(1,evt$nclu[i]))
        
        k=1
        while(k <= evt$nclu[i]){
          # data set 4b
          
          data_set_4b = rmfi_parse_variables(evt_lines)
          evt$mltarr[[i]][1,k] = as.character(data_set_4b$variables[1])
          evt$zonarr[[i]][1,k] = as.character(data_set_4b$variables[2])
          if(length(data_set_4b$variables) > 2) evt$iz[[i]][1,k] = paste(data_set_4b$variables[-c(1:2)], collapse=' ')
          
          k=k+1
          evt_lines = data_set_4b$remaining_lines
          rm(data_set_4b)
        }
        
      }
      
      i = i+1
    }
    if(all(is.na(unlist(evt$iz)))) evt$iz = NULL
  }
  
  # stress periods
  if(evt$nevtop==2){
    evt$inievt = vector(mode='numeric', length=dis$nper)
    evt$ievt = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nper))
  }
  evt$surf = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nper))
  if(((!is.null(evt$npevt) && evt$npevt==0) || is.null(evt$npevt))) evt$evtr = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nper))
  if(!is.null(evt$npevt) && evt$npevt > 0){
    evt$pname = list()
    if(!is.null(evt$instances) && T %in% evt$instances) evt$iname = list()
    evt$ievtpf = list()
  }
  evt$exdp = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nper))
  
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 = rmfi_parse_variables(evt_lines)
    evt$insurf[i] = as.numeric(data_set_5$variables[1])
    evt$inevtr[i] = as.numeric(data_set_5$variables[2])
    evt$inexdp[i] = as.numeric(data_set_5$variables[3])
    if(length(data_set_5$variables) > 3) evt$inievt[i] = as.numeric(data_set_5$variables[4])
    evt_lines = data_set_5$remaining_lines
    rm(data_set_5)
    
    # data set 6
    if(evt$insurf[i] >= 0){
      data_set_6 = rmfi_parse_array(evt_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      evt$surf[,,i] = data_set_6$array
      evt_lines = data_set_6$remaining_lines
      rm(data_set_6)
      
    }
    
    # data set 7
    if(((!is.null(evt$npevt) && evt$npevt==0) || is.null(evt$npevt)) && evt$inevtr[i] >= 0) {
      data_set_7 = rmfi_parse_array(evt_lines, nrow = dis$nrow, ncol=dis$ncol, nlay=1, file = file, ...)
      evt$evtr[,,i] = data_set_7$array
      evt_lines = data_set_7$remaining_lines
      rm(data_set_7)
    }
    
    if((!is.null(evt$npevt) && evt$npevt[i] > 0) && evt$inevtr[i] > 0){
      evt$iname[[i]] = evt$pname[[i]] = vector(length=evt$inevtr[i])
      evt$ievtpf[[i]] = vector(mode='numeric', length=evt$inevtr[i])
      
      for(j in 1:evt$inevtr[i]){
        # data set 8
        data_set_8 = rmfi_parse_variables(evt_lines)
        evt$pname[[i]][j] = as.character(data_set_8$variables[1])
        if((length(data_set_8$variables) > 1) && (!is.null(evt$instances) && evt$instances[which(evt$parnam == evt$pname[[i]][j])])){
          evt$iname[[i]][j] = as.character(data_set_8$variables[2])
        } 
        if((length(data_set_8$variables) > 1) && (is.null(evt$instances) || (!is.null(evt$instances) && !evt$instances[which(evt$parnam == evt$pname[[i]][j])]))){
          evt$ievtpf[[i]][j] = as.numeric(data_set_8$variables[2])
        } 
        evt_lines = data_set_8$remaining_lines
        rm(data_set_8)
      }
    }
    if(is.logical(unlist(evt$iname)) && !any(unlist(evt$iname))) evt$iname = NULL
    if(is.logical(unlist(evt$ievtpf)) && !any(unlist(evt$ievtpf))) evt$ievtpf = NULL
    
    # data set 9
    if(evt$inexdp[i] >= 0){
      data_set_9 = rmfi_parse_array(evt_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      evt$exdp[,,i] = data_set_9$array
      evt_lines = data_set_9$remaining_lines
      rm(data_set_9)
    }
    
    if(evt$nevtop == 2 && (!is.null(evt$inievt) && evt$inievt[i] >= 0)){
      # data set 10
      data_set_10 = rmfi_parse_array(evt_lines, nrow=dis$nrow, ncol=dis$ncol, nlay=1, file = file, ...)
      evt$ievt[,,i] = data_set_10$array
      evt_lines = data_set_10$remaining_lines
      rm(data_set_10)
    } 
    
  }
  
  class(evt) = c('evt', 'rmf_package')
  return(evt)
  
}

#' Write a MODFLOW evapotranspiration file
#'
#' \code{rmf_write_evt} writes a MODFLOW evapotranspiration file based on an \code{RMODFLOW} evt object
#' 
#' @param evt an \code{RMODFLOW} evt object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.evt'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_evt}}, \code{\link{rmf_create_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}


rmf_write_evt = function(evt, dis=rmf_read_dis(), file = {cat('Please select evt file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Evapotranspiration Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(evt)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(!is.null(evt$npevt) && evt$npevt > 0 ) rmfi_write_variables('PARAMETER', evt$npevt, file=file)
  
  # data set 2
  rmfi_write_variables(evt$nevtop, evt$ievtcb, file=file)
  
  # parameters
  if(!is.null(evt$npevt) && evt$npevt > 0){
    for (i in 1:evt$npevt){
      # data set 3
      rmfi_write_variables(evt$parnam[i], evt$partyp[i], evt$parval[i], evt$nclu[i], ifelse(evt$instances[i], 'INSTANCES', ' '), ifelse(evt$instances[i], evt$numinst[i], ' '), file=file)
      
      # time-varying
      if(!is.null(evt$instances) && evt$instances[i]){
        for (j in 1:evt$numinst[i]){
          # data set 4a
          if(evt$instances[i]) rmfi_write_variables(evt$instnam[[i]][j], file=file)
          
          # data set 4b
          for (k in 1:evt$nclu[i]){
            rmfi_write_variables(evt$mltarr[[i]][j, k], evt$zonarr[[i]][j,k], ifelse(!is.null(evt$iz) && evt$zonarr[[i]][j,k]!='ALL', evt$iz[[i]][j,k], ''), file=file)
            
          }
        }
      } else { # non-time-varying
        # data set 4b
        for (k in 1:evt$nclu[i]){
          rmfi_write_variables(evt$mltarr[[i]][1, k], evt$zonarr[[i]][1,k], ifelse(!is.null(evt$iz) && evt$zonarr[[i]][1,k]!='ALL', evt$iz[[i]][1,k], ''), file=file)
          
        }
      }
      
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    rmfi_write_variables(evt$insurf[i], evt$inevtr[i], evt$inexdp[i], ifelse(evt$nevtop == 2, evt$inievt[i], ''), file=file)
    
    # data set 6
    if(evt$insurf[i] >= 0) rmfi_write_array(evt$surf[,,i], file=file)
    
    # data set 7
    if(((!is.null(evt$npevt) && evt$npevt==0) || is.null(evt$npevt)) && evt$inevtr[i] >= 0) rmfi_write_array(evt$evtr[,,i], file=file)
    
    # data set 8
    if((!is.null(evt$npevt) && evt$npevt > 0) && evt$inevtr[i] > 0){
      for (j in 1:evt$inevtr[i]){
        
        rmfi_write_variables(evt$pname[[i]][j], ifelse(!is.null(evt$instances) && evt$instances[which(evt$parnam==evt$pname[[i]][j])], evt$iname[[i]][j], ''), ifelse((is.null(evt$instances) || (!is.null(evt$instances) && !(evt$instances[which(evt$parnam==evt$pname[[i]][j])]))) && !is.null(evt$ievtpf), evt$ievtpf[[i]][j], ' '), file=file) 
        
      }
    }
    
    # data set 9
    if(evt$inexdp[i] >= 0) rmfi_write_array(evt$exdp[,,i], file=file)
    
    # data set 10
    if(evt$nevtop==2 && (!is.null(evt$inievt) && evt$inievt[i] >= 0)){
      rmfi_write_array(evt$ievt[,,i], file=file)
    }
    
  }
  
  
}