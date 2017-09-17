#' Read a MODFLOW evapotranspiration file
#' 
#' \code{rmf_read_evt} reads in a MODFLOW evapotranspiration file and returns it as an \code{RMODFLOW} evt object.
#'
#' @param file filename; typically '*.evt'
#' @param dis an \code{RMODFLOW} dis object
#' 
#' @return \code{RMODFLOW} evt object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_evt}}, \code{\link{rmf_create_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}

rmf_read_evt = function(file = {cat('Please select evapotranspiration file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}
){
  
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
            if(length(data_set_4b$variables) > 2) evt$iz[[i]][j,k] = as.numeric(data_set_4b$variables[3])
            
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
          if(length(data_set_4b$variables) > 2) evt$iz[[i]][1,k] = as.numeric(data_set_4b$variables[3])
          
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
      data_set_6 = rmfi_parse_array(evt_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1)
      evt$surf[,,i] = data_set_6$array
      evt_lines = data_set_6$remaining_lines
      rm(data_set_6)
      
    }
    
    # data set 7
    if(((!is.null(evt$npevt) && evt$npevt==0) || is.null(evt$npevt)) && evt$inevtr[i] >= 0) {
      data_set_7 = rmfi_parse_array(evt_lines, nrow = dis$nrow, ncol=dis$ncol, nlay=1)
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
      data_set_9 = rmfi_parse_array(evt_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1)
      evt$exdp[,,i] = data_set_9$array
      evt_lines = data_set_9$remaining_lines
      rm(data_set_9)
    }
    
    if(evt$nevtop == 2 && (!is.null(evt$inievt) && evt$inievt[i] >= 0)){
      # data set 10
      data_set_10 = rmfi_parse_array(evt_lines, nrow=dis$nrow, ncol=dis$ncol, nlay=1)
      evt$ievt[,,i] = data_set_10$array
      evt_lines = data_set_10$remaining_lines
      rm(data_set_10)
    } 
    
  }
  
  class(evt) = c('evt', 'rmf_package')
  return(evt)
  
}
