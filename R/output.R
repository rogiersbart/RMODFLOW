
#' Read a MODFLOW cell-by-cell budget file
#' 
#' \code{rmf_read_cbc} reads in a MODFLOW cell-by-cell budget file
#' 
#' @param file filename; typically '*.cbc'
#' @param dis dis object.
#' @param huf huf object; optional. Provide only if huf heads are being read and \code{dis} is not NULL. See details.
#' @param oc oc object; optional. See details.
#' @param fluxes character; denotes which fluxes to read. Defaults to reading all fluxes. See details.
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @param timesteps optional integer vector specifying which time steps to read. If -1 is specified, only the last time step is read. Defaults to NULL. See details.
#' @return object of class \code{cbc} which is a list consisting of named rmf_arrays and/or data.frames. The names of the elements correspond to the fluxes.
#'
#' @details 
#' Fluxes include \code{'constant_head'}, \code{'storage'}, \code{'flow_right_face'}, \code{'flow_front_face'}, \code{'flow_lower_face'}, \code{'wells'},
#' \code{'river_leakage'}, \code{'recharge'}, \code{'drains'}, \code{'head_dep_bounds'} or any other description as written by MODFLOW.
#'  
#' If no \code{oc} object is supplied, for all array flow terms a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If a \code{oc} object is supplied, rmf_arrays of dimensions NROW x NCOL x NLAY are read and binded at each time step for which output is written. 
#' The resulting dimensions of the final arrays are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#' 
#' If the timesteps argument is supplied, it overwrites the use of the oc argument. For all array flow terms a rmf_array of dimensions NROW x NCOL x NLAY x length(timesteps) is created and filled.
#' 
#' If flows are interpolated to huf units, a \code{huf} object is to be supplied as well to dimension the array. This will only affect the constant-head and cell flow terms.
#' The final array will have NHUF layers instead of NLAY.
#'
#' @export
rmf_read_cbc <- function(file = {cat('Please select cell-by-cell budget file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         huf = NULL,
                         oc = NULL,
                         precision = 'single',
                         fluxes = 'all',
                         timesteps = NULL) {
  
  headers <- trimws(rmfd_cbc_headers)
  
  nbytes <- ifelse(precision == 'single', 4, 8) 
  binary <-  TRUE # MODFLOW cbc budget file is always binary
  if(binary) {
    con <- file(file,open='rb')
    cbc <- list()
    
    try({
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      desc <- readChar(con,nchars=16)
      trial <- 1
      fail <- c(FALSE,FALSE)
      
      # nsteps to dimension array
      if(!is.null(timesteps)) {
        oc <- NULL
        if(length(timesteps) == 1 && timesteps < 0) timesteps <- sum(dis$nstp)
      }
      if(is.null(oc)) {
        nsteps <- sum(dis$nstp)
      } else {
        if(is.null(oc$save_budget)) {
          nsteps <- length(which(oc$icbcfl == T))
        } else {
          # problem: oc records might be in non-ascending order or have non-existing time steps but output is still writen for current timestep
          # e.g. UZFtest2
          m_oc<- cbind(oc$iperoc, oc$itsoc)[oc$save_budget,]
          if(!is.matrix(m_oc)) m_oc <- matrix(m_oc, ncol = 2, byrow = TRUE)
          nsteps <- apply(m_oc, 1, function(i) rmfi_ifelse0(i[1] == 1, ifelse(i[2] > dis$nstp[i[1]], NA, i[2]), cumsum(dis$nstp)[i[1]-1]+i[2]))
          # check before going into nested for-loop
          if(any(is.na(nsteps)) || !all(diff(nsteps) >= 0)) {
            nsteps <- 0
            i <- 0
            for(k in 1:dis$nper) {
              for(l in 1:dis$nstp[k]) {
                if(m_oc[i+1,1] < k || (m_oc[i+1,1]==k && m_oc[i+1,2] < l)) {
                  i <- i + 1
                  m_oc[i,1] <- k
                  m_oc[i,2] <- l
                  nsteps <- nsteps + 1
                  
                } else if(m_oc[i+1,1]==k && m_oc[i+1,2]==l){
                  nsteps <- nsteps + 1
                  i <- i + 1
                }
              }
            }
          } else {
            nsteps <- min(sum(dis$nstp), length(which(oc$save_budget == TRUE)))
          }
        }
      }
      
      # loop
      while(length(desc!=0)) {
        
        name <- gsub(' ', '_', tolower(trimws(desc)))
        name_chck <- trimws(desc)
        if(trial == 1) {
          if(!(name_chck %in% headers)) {
            fail[1] = TRUE
          }
        } else if(trial == 2) {
          if(!(name_chck %in% headers)) {
            fail[2] = TRUE
          }
        }
        if(any(fail)) {
          stop(paste('Header descriptions do not match. Are you sure the file is', precision,'precision?'), call. = FALSE)
        }
        
        read <- ifelse((fluxes != 'all' && !(name %in% fluxes)), FALSE, TRUE) 
        ncol <- readBin(con,what='integer',n=1)
        nrow <- readBin(con,what='integer',n=1)
        nlay <- readBin(con,what='integer',n=1)
        
        if(!read) {
          if(nlay > 0) {
            invisible(readBin(con,what='numeric',n=nrow*ncol*nlay,size = nbytes))
          } else {
            itype <- readBin(con,what='integer',n=1)
            invisible(readBin(con,what='numeric',n=3,size = nbytes))
            
            if(itype==5) {
              nval <- readBin(con,what='integer',n=1)
            } else {
              nval <- 1
            }
            if(nval > 1) invisible(readChar(con,nchars=(nval-1)*16))
            
            if(itype %in% c(2,5)) { 
              nlist <- readBin(con,what='integer',n=1)
              if(nlist > 0) {
                for(nr in 1:nlist) {
                  invisible(readBin(con,what='integer',n=1))
                  invisible(readBin(con,what='numeric',n=nval,size = nbytes))
                }
              }
            }
            
            if(itype %in% c(0,1)) invisible(readBin(con,what='numeric',n=ncol*nrow*abs(nlay),size = nbytes))
            
            if(itype ==3) {
              invisible(readBin(con,what='integer',n=ncol*nrow))
              invisible(readBin(con,what='numeric',n=ncol*nrow,size = nbytes))  
            }
            
            if(itype ==4) invisible(readBin(con,what='numeric',n=ncol*nrow,size = nbytes))
          }
          
          # read
        } else {
          
          # create arrays
          nnlay <- dis$nlay
          if(!is.null(huf) && !is.null(huf$iohufflows) && huf$iohufflows > 0 && name %in% c('constant_head','flow_right_face','flow_front_face','flow_lower_face')) nnlay <- huf$nhuf
          if(is.null(cbc[[name]])) {
            cbc[[name]] <- rmf_create_array(NA, dim = c(dis$nrow, dis$ncol, nnlay, nsteps))
            attr(cbc[[name]], 'kstp') <- attr(cbc[[name]], 'kper') <- attr(cbc[[name]], 'pertim') <- attr(cbc[[name]], 'totim') <- attr(cbc[[name]], 'delt') <- attr(cbc[[name]], 'nstp') <- rep(NA, nsteps)
          }
          # set step number
          if(is.null(oc)) {
            stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
          } else {
            if(is.null(oc$save_budget)) {
              stp_nr <- length(which(oc$icbcfl[1:ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)] == T))
            } else {
              stp_nr <- length(which(oc$save_budget[1:which(m_oc[,1] == kper)[m_oc[,2][m_oc[,1] == kper] == kstp]] == T))
            }
          }
          
          # not compact
          if(nlay > 0) {
            cbc[[name]][,,,stp_nr] <- aperm(array(readBin(con,what='numeric',n=dis$nrow*dis$ncol*nnlay,size = nbytes),dim=c(dis$ncol,dis$nrow,nnlay)),c(2,1,3))
            
            # compact
          } else {
            itype <- readBin(con,what='integer',n=1)
            delt <- readBin(con,what='numeric',n=1,size = nbytes)
            pertim <- readBin(con,what='numeric',n=1,size = nbytes)
            totim <- readBin(con,what='numeric',n=1,size = nbytes)
            
            if(itype==5) {
              nval <- readBin(con,what='integer',n=1)
            } else {
              nval <- 1
            }
            if(nval > 1) {
              if(is.null(attr(cbc[[name]], 'ctmp'))) attr(cbc[[name]], 'ctmp') <- as.list(rep(NA,nsteps))
              ctmp <- rep(NA, (nval-1))
              for(nr in 1:(nval-1)) {
                ctmp[nr] <- tolower(trimws(readChar(con,nchars=16)))
              }
            }
            
            # return a rmf_list
            if(itype %in% c(2,5)) { 
              nlist <- readBin(con,what='integer',n=1)
              if(nlist > 0) {
                df <- matrix(NA,nrow=nlist,ncol=nval+1)
                for(nr in 1:nlist) {
                  df[nr,] <- c(readBin(con,what='integer',n=1),readBin(con,what='numeric',n=nval,size = nbytes))
                }
                ijk <- rmf_convert_id_to_ijk(df[,1], dis = list(nrow=nrow,ncol=ncol,nlay=abs(nlay)),type='modflow')
                if(is.null(cbc[[name]]) || is.array(cbc[[name]])) {
                  nstp <- ifelse(is.null(dis), 1, stp_nr)
                  cbc[[name]] <- as.data.frame(cbind(ijk$k,ijk$i,ijk$j,as.data.frame(df)[,-1], nstp, kper, kstp))
                  names(cbc[[name]]) <- tolower(c('k','i','j', 'value', if(nval > 1) {ctmp},'nstp', 'kper','kstp'))
                  cbc[[name]] <- rmf_create_list(cbc[[name]])
                  rm(df)
                } else {
                  nstp <- ifelse(is.null(dis), cbc[[name]][nrow(cbc[[name]]),nstp]+1, stp_nr)
                  df <- as.data.frame(cbind(ijk$k,ijk$i,ijk$j, as.data.frame(df)[,-1], nstp, kper, kstp))
                  names(df) <- tolower(c('k','i','j', 'value', if(nval > 1) {ctmp},'nstp', 'kper', 'kstp'))
                  
                  cbc[[name]] <- rbind(cbc[[name]], df)
                }
              } else {
                if(is.array(cbc[[name]])) cbc[[name]] = NULL
              }
            }
            
            if(itype %in% c(0,1)) {
              cbc[[name]][,,,stp_nr] <- aperm(array(readBin(con,what='numeric',n=dis$ncol*dis$nrow*nnlay,size = nbytes),dim=c(dis$ncol,dis$nrow,nnlay)),c(2,1,3))
            }
            if(itype == 3) {
              layer <- matrix(readBin(con,what='integer',n=ncol*nrow),ncol=ncol,nrow=nrow,byrow=TRUE)
              data <- matrix(readBin(con,what='numeric',n=ncol*nrow,size = nbytes),ncol=ncol,nrow=nrow,byrow=TRUE)
              
              cbc[[name]][,,,stp_nr] <- 0
              cbc[[name]][,,c(layer),stp_nr] <- c(data)
              
              rm(layer, data)
            }
            if(itype ==4) {
              cbc[[name]][,,1,stp_nr] <- matrix(readBin(con,what='numeric',n=dis$ncol*dis$nrow,size = nbytes),ncol=dis$ncol,nrow=dis$nrow,byrow=TRUE)
            }
          }
          
          # set  attributes
          if(!is.null(cbc[[name]])) {
            if(nlay > 0 || itype %in% c(2,5)) {
              # cbc[[name]] <- rmf_create_list(cbc[[name]], kper =  attr(cbc[[name]], 'kper'))
            } else {
              cbc[[name]] <- rmf_create_array(cbc[[name]], kper = attr(cbc[[name]], 'kper'))
            }
            
            attr(cbc[[name]], 'kstp')[stp_nr] <- kstp
            attr(cbc[[name]], 'kper')[stp_nr] <- kper
            attr(cbc[[name]], 'nstp')[stp_nr] <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper-1]+kstp)
            if(nlay < 0) {
              attr(cbc[[name]], 'pertim')[stp_nr] <- pertim
              attr(cbc[[name]], 'totim')[stp_nr] <- totim
              attr(cbc[[name]], 'delt')[stp_nr] <- delt
              if(nval > 1) attr(cbc[[name]], 'ctmp')[[stp_nr]] <- ctmp
            } else {
              stp <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper-1]+kstp)
              attr(cbc[[name]], 'pertim')[stp_nr] <- rmf_time_steps(perlen = dis$perlen[kper], tsmult = dis$tsmult[kper], nstp = dis$nstp[kper])[[2]][kstp]
              attr(cbc[[name]], 'totim')[stp_nr] <- rmf_time_steps(dis=dis)[[2]][stp]
              attr(cbc[[name]], 'delt')[stp_nr] <- rmf_time_steps(dis=dis)[[1]][stp]
            }
          }
        }
        
        kstp <- readBin(con,what='integer',n=1)
        kper <- readBin(con,what='integer',n=1)
        desc <- readChar(con,nchars=16)
        trial <- ifelse(trial == 1, 2, 0)
      }
    })
    
    close(con)
    if(!is.null(timesteps)) {
      
      timestp <- function(list_obj) {
        if(inherits(list_obj, 'data.frame')) {
          subset(list_obj, nstp %in% timesteps)
        } else if(inherits(list_obj, 'rmf_4d_array')) {
          rmf_create_array(list_obj[,,,timesteps], dim = c(dim(list_obj)[1:3], length(timesteps)))
        }
      }
      cbc <- lapply(cbc, timestp)
    }
    
    class(cbc) <- 'cbc'
    return(cbc)
    
  } else {
    stop('Code not up to date', call. = FALSE)
    #     # update this to match the above structure!
    #     cbc <- list()
    #     cbc.lines <- readr::read_lines(file)
    #     while(length(cbc.lines)!=0) {
    #       name <- substr(cbc.lines[1],25,40)
    #       cat('Processing',name,'...\n')    
    #       cbc[[name]] <- list()
    #       cbc[[name]]$sp <- as.numeric(substr(cbc.lines[1],1,12))
    #       cbc[[name]]$ts <- as.numeric(substr(cbc.lines[1],13,24))
    #       cbc[[name]]$ncols <- as.numeric(substr(cbc.lines[1],41,52))
    #       cbc[[name]]$nrows <- as.numeric(substr(cbc.lines[1],53,64))
    #       cbc[[name]]$nlays <- as.numeric(substr(cbc.lines[1],65,76))
    #       cbc.lines <- cbc.lines[-1]
    #       
    #       cbc[[name]]$code <- as.numeric(substr(cbc.lines[1],1,12))
    #       cbc[[name]]$delt <- as.numeric(substr(cbc.lines[1],13,27))
    #       cbc[[name]]$pertim <- as.numeric(substr(cbc.lines[1],28,42))
    #       cbc[[name]]$totim <- as.numeric(substr(cbc.lines[1],43,57))
    #       cbc.lines <- cbc.lines[-1]
    #       
    #       if(cbc[[name]]$code==1) {
    #         nrecords <- cbc[[name]]$ncols * cbc[[name]]$nrows * cbc[[name]]$nlays
    #         nlines <- ceiling(nrecords/5)
    #         # use rmfi_parse_array or rmfi_parse_variables instead!!
    #         dataVector <- NULL
    #         dataVector <- as.numeric(split_line_numbers(paste(cbc.lines[1:nlines],collapse=' ')))
    #         cbc[[name]]$data <- array(dataVector,dim=c(cbc[[name]]$ncols,cbc[[name]]$nrows,cbc[[name]]$nlays))
    #         cbc[[name]]$data <- aperm(cbc[[name]]$data,c(2,1,3))
    #         names(cbc[[name]]$data) <- c('ID','FLUX')
    #         cbc.lines <- cbc.lines[-c(1:nlines)]
    #         cbc.lines <- cbc.lines[-1]
    #       }
    #       if(cbc[[name]]$code==2) {
    #         nrecords <- as.numeric(rmfi_remove_empty_strings(strsplit(cbc.lines[1],' ')[[1]]))
    #         cbc.lines <- cbc.lines[-1]
    #         # use rmfi_parse_array or rmfi_parse_variables instead!!
    #         dataVector <- as.numeric(split_line_numbers(paste(cbc.lines[1:nrecords],collapse=' ')))
    #         cbc[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=2,byrow=T))
    #         names(cbc[[name]]$data) <- c('ID','FLUX')
    #         cbc.lines <- cbc.lines[-c(1:nrecords)]
    #         cbc.lines <- cbc.lines[-1]
    #       }
    #       if(cbc[[name]]$code==3 | cbc[[name]]$code==4) {
    #         nrecords <- cbc[[name]]$ncols * cbc[[name]]$nrows
    #         nlines <- ceiling(nrecords/5)
    #         dataVector <- NULL
    #         # use rmfi_parse_array or rmfi_parse_variables instead!!
    #         dataVector <- as.numeric(split_line_numbers(paste(cbc.lines[1:nlines],collapse=' ')))
    #         cbc[[name]]$data <- matrix(dataVector,ncol=cbc[[name]]$ncols,nrow=cbc[[name]]$nrows,byrow=T)
    #         cbc.lines <- cbc.lines[-c(1:nlines)]
    #         cbc.lines <- cbc.lines[-1]
    #       }
    #       if(cbc[[name]]$code==5) {
    #         nvalues <- as.numeric(rmfi_remove_empty_strings(strsplit(cbc.lines[1],' ')[[1]]))
    #         cbc.lines <- cbc.lines[-1]
    #         if(nvalues > 1) {
    #           additionalColumns <- rep(NA,nvalues-1)
    #           for(i in 1:(nvalues-1)) {
    #             additionalColumns[i] <- rmfi_remove_empty_strings(strsplit(cbc.lines[1],' ')[[1]])
    #             cbc.lines <- cbc.lines[-1]
    #           }
    #           #cbc.lines <- cbc.lines[-1] #IFACE
    #           #cbc.lines <- cbc.lines[-1] #CONDFACT
    #           #cbc.lines <- cbc.lines[-1] #CELLGRP
    #         }
    #         nrecords <- as.numeric(rmfi_remove_empty_strings(strsplit(cbc.lines[1],' ')[[1]]))
    #         cbc.lines <- cbc.lines[-1]
    #         # use rmfi_parse_array or rmfi_parse_variables instead!!
    #         dataVector <- as.numeric(split_line_numbers(paste(cbc.lines[1:nrecords],collapse=' ')))
    #         cbc[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=nvalues+1,byrow=T))
    #         if(nvalues > 1) names(cbc[[name]]$data) <- c('ID','FLUX',additionalColumns)
    #         if(nvalues == 1)names(cbc[[name]]$data) <- c('ID','FLUX')
    #         cbc.lines <- cbc.lines[-c(1:nrecords)]
    #         cbc.lines <- cbc.lines[-1]
    #       }
    #     }
    #     class(cbc) <- 'cbc'
    #     return(cbc)
  }
}

#' Read a MODFLOW head file
#' 
#' \code{rmf_read_hed} reads in a MODFLOW head file and returns it as an \code{RMODFLOW} hed object.
#' 
#' @param file filename; typically '*.hed'
#' @param dis dis object
#' @param huf huf object; optional. Provide only if huf heads are being read. See details.
#' @param oc oc object; optional. See details.
#' @param bas bas object; optional. If supplied, is used to set the hnoflo values to NA.
#' @param hdry numeric value as set by the flow package (bcf, lpf, huf or upw). If supplied, cells with this value are dry and their values are set to NA. Defaults to NULL (unless huf is supplied)
#' @param binary logical; is the file binary?
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @param timesteps optional integer vector specifying which time steps to read. If -1 is specified, only the last time step is read. Defaults to NULL. See details.
#' @return object of class hed and rmf_4d_array.
#' @details 
#' When huf heads are to be read, a \code{huf} object should also be supplied. The final array will have NHUF layers instead of NLAY.
#' 
#' If no \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If a \code{oc} object is supplied, the dimensions of the returned array are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#' If the array is in ASCII format and no headers are present, a \code{OC} object must be supplied. In that case, it is assumed that the file being read only contains head values and the keyword XSECTION in the bas file is not present.
#' 
#' If the timesteps argument is supplied, it overwrites the use of the oc argument. For all array flow terms a rmf_array of dimensions NROW x NCOL x NLAY x length(timesteps) is created and filled.
#' 
#' The only use of the bas argument is to replace the hnoflo values in the final array with NA's. 
#'
#' @export
rmf_read_hed <- function(file = {cat('Please select head file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         huf = NULL,
                         oc = NULL,
                         bas = NULL,
                         hdry = huf$hdry,
                         binary = TRUE,
                         precision = 'single',
                         timesteps = NULL) {
  
  headers <- trimws(rmfd_state_headers)
  other_desc <- NULL

  if(binary) { # Binary
    
    real_number_bytes <- ifelse(precision == 'single', 4, 8)
    con <- file(file,open='rb')
    
    try({   
      
      if(!is.null(huf) && huf$iohufheads > 0) {
        dis$nlay <- huf$nhuf
      }
      
      if(!is.null(timesteps)) {
        oc <- NULL
        if(length(timesteps) == 1 && timesteps < 0) timesteps <- sum(dis$nstp)
      }
      # check time steps if oc is specified
      if(!is.null(oc)) {
        # oc using words
        if(!is.null(oc$save_head)) {
          # problem: oc records might be in non-ascending order or have non-existing time steps but output is still writen for current timestep
          # e.g. UZFtest2
          m_oc<- cbind(oc$iperoc, oc$itsoc)[rmfi_ifelse0(is.matrix(oc$save_head), apply(oc$save_head, 2, any), oc$save_head),] 
          if(!is.matrix(m_oc)) m_oc <- matrix(m_oc, ncol = 2, byrow = TRUE)
          nsteps <- apply(m_oc, 1, function(i) rmfi_ifelse0(i[1] == 1, ifelse(i[2] > dis$nstp[i[1]], NA, i[2]), cumsum(dis$nstp)[i[1]-1]+i[2]))
          # check before going into nested for-loop
          if(any(is.na(nsteps)) || !all(diff(nsteps) >= 0)) {
            nsteps <- 0
            i <- 0
            for(k in 1:dis$nper) {
              for(l in 1:dis$nstp[k]) {
                if(m_oc[i+1,1] < k || (m_oc[i+1,1]==k && m_oc[i+1,2] < l)) {
                  i <- i + 1
                  m_oc[i,1] <- k
                  m_oc[i,2] <- l
                  nsteps <- nsteps + 1
                  
                } else if(m_oc[i+1,1]==k && m_oc[i+1,2]==l){
                  nsteps <- nsteps + 1
                  i <- i + 1
                }
              }
            }
          } else {
            nsteps <- min(sum(dis$nstp), length(which(oc$save_head == TRUE)))
          }
          # oc using codes
        } else {
          nsteps <- length(which(apply(oc$hdsv,1,function(i) any(i==TRUE))==TRUE))
        }
      } else {
        nsteps <- sum(dis$nstp)
      }
      
      first <- TRUE  
      
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      desc <- trimws(readChar(con,nchars=16))
      if(! desc %in% headers) {
        stop('Array description not recognized. Is the file really binary? If so, you could try double precision. If not, set the binary argument to FALSE.', call. = FALSE)
      }
      while(length(desc != 0)) {
        
        name <- gsub(' ', '_', tolower(desc))
        
        # if IOHUFHEADS > 0, there will also be normal head per layer arrays. Do not return those.
        if(!is.null(huf) && huf$iohufheads > 0 && desc != 'HEAD IN HGU'){
          other_desc <- append(other_desc, desc)
          invisible(readBin(con, what='integer', n=3))
          invisible(readBin(con,what='numeric',n = dis$ncol * dis$nrow, size = real_number_bytes))
          invisible(readBin(con, what='integer', n=2))
          invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
          desc <-  trimws(readChar(con,nchars=16))
          
        } else if(is.null(huf) && name != 'head') {
          other_desc <- append(other_desc, name)
          invisible(readBin(con, what='integer', n=3))
          invisible(readBin(con,what='numeric',n = dis$ncol * dis$nrow, size = real_number_bytes))
          invisible(readBin(con, what='integer', n=2))
          invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
          desc <-  trimws(readChar(con,nchars=16))
          
        } else {
          
          ncol <- readBin(con, what = 'integer', n = 1)
          nrow <- readBin(con, what = 'integer', n = 1)
          ilay <- readBin(con, what = 'integer', n = 1)
          # xsection - ilay never negative with huf. Only do this once.
          xsection <- FALSE
          if(ilay < 0 && first) {
            xsection <- TRUE
            dis$nrow <- dis$nlay
            dis$nlay <- 1
          }
          ilay <- abs(ilay)
          
          if(first) {
            hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, nsteps))
            attr(hed, 'kstp') <- attr(hed, 'kper') <-  attr(hed, 'pertim') <-  attr(hed, 'totim') <- attr(hed, 'nstp') <- rep(NA, nsteps)
          }
          if(is.null(oc)) {
            stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
          } else {
            if(first) {
              stp_nr <- 1
            } else {
              stp_nr <- ifelse(ilay == 1, stp_nr+1, stp_nr)
            }
          }
          
          hed[,,ilay,stp_nr] <- aperm(array(readBin(con,what='numeric',n = ncol * nrow, size = real_number_bytes),dim=c(ncol, nrow)), c(2, 1))
          
          attr(hed,'kstp')[stp_nr] <- kstp
          attr(hed,'kper')[stp_nr] <- kper
          attr(hed,'pertim')[stp_nr] <- pertim
          attr(hed,'totim')[stp_nr] <- totim
          attr(hed, 'nstp')[stp_nr] <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper-1]+kstp)
          
          
          first <- FALSE
          kstp <- readBin(con,what='integer',n=1)
          kper <- readBin(con,what='integer',n=1)
          pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          desc <- trimws(readChar(con,nchars=16))
        }
      }
      
      if(!is.null(bas)) hed[which(hed == bas$hnoflo)] <-  NA
      if(!is.null(hdry)) hed[which(hed == hdry)] <- NA
      
      if(!is.null(other_desc) && length(other_desc) != 0) {
        warning(paste('HEAD or HEAD IN HGU not found in file. Found ', length(other_desc), 'other descriptions:','\n',paste(other_desc, '\n'),'\n','Returning NULL'), call. = FALSE)
        hed <- NULL
      } else {
        hed <- rmf_create_array(hed, dimlabels = rmfi_ifelse0(xsection, c('k', 'j', 'i', 'l'), c('i', 'j', 'k', 'l')))
        if(!is.null(timesteps)) {
          hed <- rmf_create_array(hed[,,,timesteps], dim = c(dim(hed)[1:3], length(timesteps)))
        }
        class(hed) <- c('hed', class(hed))
      }
    })
    close(con)
    return(hed)
    
  } else { # ASCII
    hed.lines <- readr::read_lines(file)
    label <- TRUE
    header_fmt <- '(1X,2I5,2E15.6,A17,3I6,A17)'
    header_fmt <- rmfi_fortran_format(header_fmt)
    
    variables <- trimws(substring(hed.lines[1], cumsum(header_fmt)[-length(header_fmt)] + 1, cumsum(header_fmt)[-1]))
    # variables <- rmfi_remove_empty_strings(strsplit(variables,' ')[[1]])
    desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
    if(! desc %in% headers) {
      if(variables[2] != dis$ncol) { # weak test to check if there's a label
        stop('Array description not recognized. Are you sure the file is not binary ?', call. = FALSE)
      }
      label <- FALSE
      if(is.null(oc)) {
        stop('No label line detected. Please specify an OC object.', call. = FALSE)
      } else {
        warning('Assuming file being read only contains head values.', call. = FALSE)
      }
    }
    
    
    # skip non-head arrays
    if(label) {
      if(!is.null(huf) && huf$iohufheads > 0){
        
        if(any(grepl('HEAD IN HGU', hed.lines))) {
          hed.lines <-  hed.lines[grep('HEAD IN HGU', hed.lines)[1]:length(hed.lines)]
          dis$nlay <-  huf$nhuf
        } else {
          other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = FALSE)]
          hed.lines <-  NULL
        }
      } else {
        if(any(grepl('HEAD', hed.lines))) {
          hed.lines <-  hed.lines[grep('HEAD', hed.lines)[1]:length(hed.lines)]
        } else {
          other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = FALSE)]
          hed.lines <-  NULL
        }
      }
    }
    
    if(!is.null(timesteps)) {
      oc <- NULL
      if(length(timesteps) == 1 && timesteps < 0) timesteps <- sum(dis$nstp)
    }
    # check time steps if oc is specified
    if(!is.null(oc)) {
      # oc using words
      if(!is.null(oc$save_head) ) {
        if(is.matrix(oc$save_head)) {
          nsteps <- length(which(apply(oc$save_head,2,function(i) any(i==TRUE))==TRUE))
        } else {
          nsteps <- length(which(oc$save_head == TRUE))
        }
        # oc using codes
      } else {
        nsteps <- length(which(apply(oc$hdsv,2,function(i) any(i==TRUE))==TRUE))
      }
    } else {
      nsteps <- sum(dis$nstp)
    }
    
    first <- TRUE
    while(length(hed.lines) != 0) {
      
      if(label) {
        # variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
        variables <- trimws(substring(hed.lines[1], cumsum(header_fmt)[-length(header_fmt)] + 1, cumsum(header_fmt)[-1]))
        kstp <- as.numeric(variables[1])
        kper <- as.numeric(variables[2])
        pertim <- as.numeric(variables[3])
        totim <- as.numeric(variables[4])
        desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
        fmt <- variables[length(variables)]
        if(! desc %in% headers) {
          stop('Array description not recognized. Are you sure the file is not binary ?', call. = FALSE)
        }
        name <- gsub(' ', '_', tolower(trimws(desc)))
        
        ncol <- as.numeric(variables[length(variables)-3])
        nrow <- as.numeric(variables[length(variables)-2])
        ilay <- abs(as.numeric(variables[length(variables)-1]))
        hed.lines <- hed.lines[-1]
        
        # xsection - ilay never negative with huf. Only do this once.
        xsection <- FALSE
        if(ilay < 0 && !is.null(dis) && first) {
          xsection <- TRUE
          dis$nrow <- dis$nlay
          dis$nlay <- 1
        }
        ilay <- abs(ilay)
        
        # no label
      } else {
        
        # oc words
        if(!is.null(oc$save_head)) {
          fmt <- oc$chedfm
          
          if(is.matrix(oc$save_head)) {
            ind <- ifelse(first, 1, ind + 1)
            read <- oc$save_head[ind]
            ilay <- arrayInd(ind, dim(oc$save_head))[1]
            kstp <- oc$itsoc[arrayInd(ind, dim(oc$save_head))[2]]
            kper <- oc$iperoc[arrayInd(ind, dim(oc$save_head))[2]]
            pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:(cumsum(dis$nstp)[kper]+kstp)])
            totim <- sum(rmf_time_steps(dis=dis)$tsl[1:(cumsum(dis$nstp)[kper]+kstp)])
          } else {
            ind <- ifelse(first, 1, ifelse(ilay == dis$nlay, ind, ind + 1))
            read <- oc$save_head[ind]
            if(read) {
              if(first) {
                ilay <- 1
              } else {
                ilay <- ifelse(ilay != dis$nlay, ilay + 1, 1)
              }
              kstp <- oc$itsoc[ind]
              kper <- oc$iperoc[ind]
              pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:(cumsum(dis$nstp)[kper]+kstp)])
              totim <- sum(rmf_time_steps(dis=dis)$tsl[1:(cumsum(dis$nstp)[kper]+kstp)])
            }
          }
          
          # oc codes; not possible if output file is ASCII
        } else if(!is.null(oc$hdsv)) {
          ind <- ifelse(first, 1, ind + 1)
          read <- oc$hdsv[ind]
          ilay <- arrayInd(ind, dim(oc$hdsv))[1]
          nstp <-  arrayInd(ind, dim(oc$hdsv))[2]
          kper <- findInterval(nstp, cumsum(dis$nstp), left.open = T) + 1
          kstp <- rmfi_ifelse0(kper == 1, nstp, nstp - cumsum(dis$nstp)[kper - 1])
          pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:nstp])
          totim <- sum(rmf_time_steps(dis=dis)$tsl[1:nstp])
        }
        
      }
      
      # read array
      data_set <- rmfi_parse_array(hed.lines,dis$nrow,dis$ncol,1, ndim = 2, skip_header = TRUE, fmt = fmt)
      
      if(first) {
        hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, nsteps))
        attr(hed, 'kstp') <- attr(hed, 'kper') <-  attr(hed, 'pertim') <-  attr(hed, 'totim') <- attr(hed, 'nstp') <- rep(NA, nsteps)
      }
      if(is.null(oc)) {
        stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
      } else {
        if(first) {
          stp_nr <- 1
        } else {
          stp_nr <- ifelse(ilay == 1, stp_nr+1, stp_nr)
        }       
      }
      hed[,,ilay,stp_nr] <- data_set$array
      
      attr(hed,'kstp')[stp_nr] <- kstp
      attr(hed,'kper')[stp_nr] <- kper
      attr(hed,'pertim')[stp_nr] <- pertim
      attr(hed,'totim')[stp_nr] <- totim
      attr(hed, 'nstp')[stp_nr] <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper-1]+kstp)
      
      
      first <- FALSE
      hed.lines <- data_set$remaining_lines
      
      # skip non-head arrays
      if(!is.null(huf) && huf$iohufheads > 0){
        
        if(any(grepl('HEAD IN HGU', hed.lines))) {
          hed.lines <-  hed.lines[grep('HEAD IN HGU', hed.lines)[1]:length(hed.lines)]
          dis$nlay <-  huf$nhuf
        } else {
          other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
          hed.lines <-  NULL
        }
      } else {
        if(any(grepl('HEAD', hed.lines))) {
          hed.lines <-  hed.lines[grep('HEAD', hed.lines)[1]:length(hed.lines)]
        } else {
          other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
          hed.lines <-  NULL
        }
      }
      
    }
    
    if(!is.null(bas)) hed[which(hed == bas$hnoflo)] <-  NA
    if(!is.null(hdry)) hed[which(hed == hdry)] <- NA
    
    if(!is.null(other_desc) && length(other_desc) != 0) {
      warning(paste('HEAD or HEAD IN HGU not found in file. Found ', length(other_desc), 'other descriptions:','\n',paste(other_desc, '\n'),'\n','Returning NULL'), call. = FALSE)
      return(NULL)
    } else {
      hed <- rmf_create_array(hed, dimlabels = rmfi_ifelse0(xsection, c('k', 'j', 'i', 'l'), c('i', 'j', 'k', 'l')))
      if(!is.null(timesteps)) {
        hed <- rmf_create_array(hed[,,,timesteps], dim = c(dim(hed)[1:3], length(timesteps)))
      }
      class(hed) <- c('hed', class(hed))
      return(hed)
    }
  }
}

#' @describeIn rmf_read_hed 
#' @export
rmf_read_head <- function(...) {
  rmf_read_hed(...)
}

#' @describeIn rmf_read_hed Compatible with default ModelMuse file extension
#' @export
rmf_read_fhd <- function(...) {
  rmf_read_hed(..., binary = FALSE)
}

#' @describeIn rmf_read_hed Compatible with default ModelMuse file extension
#' @export
rmf_read_bhd <- function(...) {
  rmf_read_hed(..., binary = TRUE)
}

#' Read a MODFLOW drawdown file
#' 
#' \code{rmf_read_ddn} reads in a MODFLOW drawdown file and returns it as an \code{RMODFLOW} ddn object.
#' 
#' @param file filename; typically '*.ddn'
#' @param dis dis object
#' @param oc oc object; optional. See details.
#' @param bas bas object; optional. If supplied, is used to set the hnoflo values to NA.
#' @param hdry numeric value as set by the flow package (bcf, lpf, huf or upw). If supplied, cells with this value are dry and their values are set to NA. Defaults to NULL
#' @param binary logical; is the file binary?
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @param timesteps optional integer vector specifying which time steps to read. If -1 is specified, only the last time step is read. Defaults to NULL. See details.
#' @return object of class ddn and rmf_4d_array
#' @details 
#' 
#' If no \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If a \code{oc} object is supplied, the dimensions of the returned array are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#' If the array is in ASCII format and no headers are present, a \code{OC} object must be supplied. In that case, it is assumed that the file being read only contains head values and the keyword XSECTION in the bas file is not present.
#' 
#' If the timesteps argument is supplied, it overwrites the use of the oc argument. For all array flow terms a rmf_array of dimensions NROW x NCOL x NLAY x length(timesteps) is created and filled.
#' 
#' The only use of the bas argument is to replace the hnoflo values in the final array with NA's. 
#'
#' @export
rmf_read_ddn <- function(file = {cat('Please select ddn file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         oc = NULL,
                         bas = NULL,
                         hdry = NULL,
                         binary = TRUE,
                         precision = 'single',
                         timesteps = NULL) {
  
  headers <- rmfd_state_headers
  other_desc <- NULL
  
  if(binary) { # Binary
    
    real_number_bytes <- ifelse(precision == 'single', 4, 8)
    con <- file(file,open='rb')
    
    try({   
      
      # if(!is.null(huf) && huf$iohufheads > 0) {
      #   dis$nlay <- huf$nhuf
      # }
      
      if(!is.null(timesteps)) {
        oc <- NULL
        if(length(timesteps) == 1 && timesteps < 0) timesteps <- sum(dis$nstp)
      }
      # check time steps if oc is specified
      if(!is.null(oc)) {
        # oc using words
        if(!is.null(oc$save_drawdown) ) {
          # problem: oc records might be in non-ascending order or have non-existing time steps but output is still writen for current timestep
          # e.g. UZFtest2
          m_oc<- cbind(oc$iperoc, oc$itsoc)[rmfi_ifelse0(is.matrix(oc$save_drawdown), apply(oc$save_drawdown, 2, any), oc$save_drawdown),]
          if(!is.matrix(m_oc)) m_oc <- matrix(m_oc, ncol = 2, byrow = TRUE)
          nsteps <- apply(m_oc, 1, function(i) rmfi_ifelse0(i[1] == 1, ifelse(i[2] > dis$nstp[i[1]], NA, i[2]), cumsum(dis$nstp)[i[1]-1]+i[2]))
          # check before going into nested for-loop
          if(any(is.na(nsteps)) || !all(diff(nsteps) >= 0)) {
            nsteps <- 0
            i <- 0
            for(k in 1:dis$nper) {
              for(l in 1:dis$nstp[k]) {
                if(m_oc[i+1,1] < k || (m_oc[i+1,1]==k && m_oc[i+1,2] < l)) {
                  i <- i + 1
                  m_oc[i,1] <- k
                  m_oc[i,2] <- l
                  nsteps <- nsteps + 1
                  
                } else if(m_oc[i+1,1]==k && m_oc[i+1,2]==l){
                  nsteps <- nsteps + 1
                  i <- i + 1
                }
              }
            }
          } else {
            nsteps <- min(sum(dis$nstp), length(which(oc$save_drawdown == TRUE)))
          }
          
          # oc using codes
        } else {
          nsteps <- length(which(apply(oc$ddsv,1,function(i) any(i==TRUE))==TRUE))
        }
      } else {
        nsteps <- sum(dis$nstp)
      }
      
      first <- TRUE  
      
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      desc <- trimws(readChar(con,nchars=16))
      if(! desc %in% headers) {
        stop('Array description not recognized. Is the file really binary? If so, you could try double precision. If not, set the binary argument to FALSE.', call. = FALSE)
      }
      while(length(desc != 0)) {
        
        name <- gsub(' ', '_', tolower(desc))
        
        # # if IOHUFHEADS > 0, there will also be normal head per layer arrays. Do not return those.
        # if(!is.null(huf) && huf$iohufheads > 0 && desc != '     HEAD IN HGU'){
        #    other_desc <- append(other_desc, desc)
        #    invisible(readBin(con, what='integer', n=3))
        #    invisible(readBin(con,what='numeric',n = dis$ncol * dis$nrow, size = real_number_bytes))
        #    invisible(readBin(con, what='integer', n=2))
        #    invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
        #    desc = readChar(con,nchars=16)
        #   
        # } else 
        if(name != 'drawdown') {
          other_desc <- append(other_desc, name)
          invisible(readBin(con, what='integer', n=3))
          invisible(readBin(con,what='numeric',n = dis$ncol * dis$nrow, size = real_number_bytes))
          invisible(readBin(con, what='integer', n=2))
          invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
          desc <-  trimws(readChar(con,nchars=16))
          
        } else {
          
          ncol <- readBin(con, what = 'integer', n = 1)
          nrow <- readBin(con, what = 'integer', n = 1)
          ilay <- readBin(con, what = 'integer', n = 1)
          # xsection - ilay never negative with huf. Only do this once.
          xsection <- FALSE
          if(ilay < 0 && first) {
            xsection <- TRUE
            dis$nrow <- dis$nlay
            dis$nlay <- 1
          }
          ilay <- abs(ilay)
          
          if(first) {
            hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, nsteps))
            attr(hed, 'kstp') <- attr(hed, 'kper') <-  attr(hed, 'pertim') <-  attr(hed, 'totim') <- attr(hed, 'nstp') <- rep(NA, nsteps)
          }
          if(is.null(oc)) {
            stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
          } else {
            if(first) {
              stp_nr <- 1
            } else {
              stp_nr <- ifelse(ilay == 1, stp_nr+1, stp_nr)
            }
          }
          
          hed[,,ilay,stp_nr] <- aperm(array(readBin(con,what='numeric',n = ncol * nrow, size = real_number_bytes),dim=c(ncol, nrow)), c(2, 1))
          
          attr(hed,'kstp')[stp_nr] <- kstp
          attr(hed,'kper')[stp_nr] <- kper
          attr(hed,'pertim')[stp_nr] <- pertim
          attr(hed,'totim')[stp_nr] <- totim
          attr(hed,'nstp')[stp_nr] <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper-1]+kstp)
          
          first <- FALSE
          kstp <- readBin(con,what='integer',n=1)
          kper <- readBin(con,what='integer',n=1)
          pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          desc <- trimws(readChar(con,nchars=16))
        }
      }
      
      if(!is.null(other_desc) && length(other_desc) != 0) {
        warning(paste('DRAWDOWN not found in file. Found ', length(other_desc), 'other descriptions:','\n',paste(other_desc, '\n'),'\n','Returning NULL'), call. = FALSE)
        hed <- NULL
      } else {
        if(!is.null(bas)) hed[which(hed == bas$hnoflo)] <-  NA
        if(!is.null(hdry)) hed[which(hed == hdry)] <- NA

        hed <- rmf_create_array(hed, dimlabels = rmfi_ifelse0(xsection, c('k', 'j', 'i', 'l'), c('i', 'j', 'k', 'l')))
        if(!is.null(timesteps)) {
          hed <- rmf_create_array(hed[,,,timesteps], dim = c(dim(hed)[1:3], length(timesteps)))
        }
        class(hed) <- c('ddn', class(hed))
      }
    })
    close(con)
    return(hed)
    
  } else { # ASCII
    hed.lines <- readr::read_lines(file)
    label <- TRUE
    header_fmt <- '(1X,2I5,2E15.6,A17,3I6,A17)'
    header_fmt <- rmfi_fortran_format(header_fmt)
    
    variables <- trimws(substring(hed.lines[1], cumsum(header_fmt)[-length(header_fmt)] + 1, cumsum(header_fmt)[-1]))
    # variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
    desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
    if(! desc %in% headers) {
      if(variables[2] != dis$ncol) { # weak test to check if there's a label
        stop('Array description not recognized. Are you sure the file is not binary ?', call. = FALSE)
      }
      label <- FALSE
      if(is.null(oc)) {
        stop('No label line detected. Please specify an OC object.', call. = FALSE)
      } else {
        warning('Assuming file being read only contains head values.', call. = FALSE)
      }
    }
    
    # skip non-drawdown arrays
    if(label) {
      # if(!is.null(huf) && huf$iohufheads > 0){
      #   if(!any(grepl('HEAD IN HGU', hed.lines))) other_desc <- headers[which(headers %in% hed.lines)]
      #   dis$nlay = huf$nhuf
      #   hed.lines = hed.lines[grep('HEAD IN HGU', hed.lines)[1]:length(hed.lines)]
      # } else {
      if(any(grepl('DRAWDOWN', hed.lines))) {
        hed.lines <-  hed.lines[grep('DRAWDOWN', hed.lines)[1]:length(hed.lines)]
      } else {
        other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
        hed.lines <-  NULL
      }
      #   }
    }
    
    if(!is.null(timesteps)) {
      oc <- NULL
      if(length(timesteps) == 1 && timesteps < 0) timesteps <- sum(dis$nstp)
    }
    # check time steps if oc is specified
    if(!is.null(oc)) {
      # oc using words
      if(!is.null(oc$save_drawdown) ) {
        if(is.matrix(oc$save_drawdown)) {
          nsteps <- length(which(apply(oc$save_drawdown,2,function(i) any(i==TRUE))==TRUE))
        } else {
          nsteps <- length(which(oc$save_drawdown == TRUE))
        }
        # oc using codes
      } else {
        nsteps <- length(which(apply(oc$ddsv,2,function(i) any(i==TRUE))==TRUE))
      }
    } else {
      nsteps <- sum(dis$nstp)
    }
    
    first <- TRUE
    while(length(hed.lines) != 0) {
      
      if(label) {
        # variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
        variables <- trimws(substring(hed.lines[1], cumsum(header_fmt)[-length(header_fmt)] + 1, cumsum(header_fmt)[-1]))
        kstp <- as.numeric(variables[1])
        kper <- as.numeric(variables[2])
        pertim <- as.numeric(variables[3])
        totim <- as.numeric(variables[4])
        desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
        fmt <- variables[length(variables)]
        if(! desc %in% headers) {
          stop('Array description not recognized. Are you sure the file is not binary ?', call. = FALSE)
        }
        name <- gsub(' ', '_', tolower(trimws(desc)))
        
        ncol <- as.numeric(variables[length(variables)-3])
        nrow <- as.numeric(variables[length(variables)-2])
        ilay <- abs(as.numeric(variables[length(variables)-1]))
        hed.lines <- hed.lines[-1]
        
        # xsection - ilay never negative with huf. Only do this once.
        xsection <- FALSE
        if(ilay < 0 && !is.null(dis) && first) {
          xsection <- TRUE
          dis$nrow <- dis$nlay
          dis$nlay <- 1
        }
        ilay <- abs(ilay)
        
        # no label
      } else {
        
        # oc words
        if(!is.null(oc$save_drawdown)) {
          fmt <- oc$cddnfm
          
          if(is.matrix(oc$save_drawdown)) {
            ind <- ifelse(first, 1, ind + 1)
            read <- oc$save_drawdown[ind]
            ilay <- arrayInd(ind, dim(oc$save_drawdown))[1]
            kstp <- oc$itsoc[arrayInd(ind, dim(oc$save_drawdown))[2]]
            kper <- oc$iperoc[arrayInd(ind, dim(oc$save_drawdown))[2]]
            pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:(cumsum(dis$nstp)[kper]+kstp)])
            totim <- sum(rmf_time_steps(dis=dis)$tsl[1:(cumsum(dis$nstp)[kper]+kstp)])
          } else {
            ind <- ifelse(first, 1, ifelse(ilay == dis$nlay, ind, ind + 1))
            read <- oc$save_drawdown[ind]
            if(read) {
              if(first) {
                ilay <- 1
              } else {
                ilay <- ifelse(ilay != dis$nlay, ilay + 1, 1)
              }
              kstp <- oc$itsoc[ind]
              kper <- oc$iperoc[ind]
              pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:(cumsum(dis$nstp)[kper]+kstp)])
              totim <- sum(rmf_time_steps(dis=dis)$tsl[1:(cumsum(dis$nstp)[kper]+kstp)])
            }
          }
          
          # oc codes; not possible if output file is ASCII
        } else if(!is.null(oc$ddsv)) {
          ind <- ifelse(first, 1, ind + 1)
          read <- oc$ddsv[ind]
          ilay <- arrayInd(ind, dim(oc$ddsv))[1]
          nstp <-  arrayInd(ind, dim(oc$ddsv))[2]
          kper <- findInterval(nstp, cumsum(dis$nstp), left.open = T) + 1
          kstp <- rmfi_ifelse0(kper == 1, nstp, nstp - cumsum(dis$nstp)[kper - 1])
          pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:nstp])
          totim <- sum(rmf_time_steps(dis=dis)$tsl[1:nstp])
        }
        
      }
      
      # read array
      data_set <- rmfi_parse_array(hed.lines,dis$nrow,dis$ncol,1, ndim = 2, skip_header = TRUE, fmt = fmt)
      
      if(first) {
        hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, nsteps))
        attr(hed, 'kstp') <- attr(hed, 'kper') <-  attr(hed, 'pertim') <-  attr(hed, 'totim') <- rep(NA, nsteps)
      }
      if(is.null(oc)) {
        stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
      } else {
        if(first) {
          stp_nr <- 1
        } else {
          stp_nr <- ifelse(ilay == 1, stp_nr+1, stp_nr)
        }        
      }
      hed[,,ilay,stp_nr] <- data_set$array
      
      attr(hed,'kstp')[stp_nr] <- kstp
      attr(hed,'kper')[stp_nr] <- kper
      attr(hed,'pertim')[stp_nr] <- pertim
      attr(hed,'totim')[stp_nr] <- totim
      
      first <- FALSE
      hed.lines <- data_set$remaining_lines
      
      # skip non-drawdown arrays
      if(any(grepl('DRAWDOWN', hed.lines))) {
        hed.lines <-  hed.lines[grep('DRAWDOWN', hed.lines)[1]:length(hed.lines)]
      } else {
        other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
        hed.lines <-  NULL
      }
      
    }
    if(!is.null(other_desc) && length(other_desc) != 0) {
      warning(paste('DRAWDOWN not found in file. Found ', length(other_desc), 'other descriptions:','\n',paste(other_desc, '\n'),'\n','Returning NULL'), call. = FALSE)
      return(NULL)
    } else {
      if(!is.null(bas)) hed[which(hed == bas$hnoflo)] <-  NA
      if(!is.null(hdry)) hed[which(hed == hdry)] <- NA
      
      hed <- rmf_create_array(hed, dimlabels = rmfi_ifelse0(xsection, c('k', 'j', 'i', 'l'), c('i', 'j', 'k', 'l')))
      if(!is.null(timesteps)) {
        hed <- rmf_create_array(hed[,,,timesteps], dim = c(dim(hed)[1:3], length(timesteps)))
      }
      class(hed) <- c('ddn', class(hed))
      return(hed)
    }
  }
}

#' @rdname rmf_read_ddn
#' @export
rmf_read_drawdown <- function(...) {
  rmf_read_ddn(...)
}

#' Reads the volumetric budget from a MODFLOW listing file
#'
#' \code{rmf_read_bud} reads a volumetric budget from a MODFLOW listing file and returns it as a list with data frame elements
#' 
#' @param file path to the listing file; typically '*.lst'
#'
#' @return an object of class bud which is a list with two data frames: one with cumulative fluxes and one with rates
#' @export

rmf_read_bud <-  function(file = {cat('Please select listing file ...\n'); file.choose()}){
  
  lst.lines <- readr::read_lines(file)
  headers <- grep("VOLUMETRIC BUDGET FOR ENTIRE MODEL", lst.lines)
  enders <- grep("TIME SUMMARY AT END OF TIME STEP", lst.lines)
  
  # if budget is printed
  if(length(headers) > 0) {
    
    # helper functions
    read_vars <- function(index, lines) rmfi_remove_empty_strings(strsplit(gsub('=', ' = ', lines[index]), ' ')[[1]])
    get_timing <- function(header_vector) {
      kstp <- as.numeric(strsplit(header_vector[11],',')[[1]])
      kper <- as.numeric(header_vector[length(header_vector)])
      # nstp <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper - 1] + kstp)
      return(list(kstp, kper))
    }
    get_vars <- function(var_vector) {
      breaks <- which(var_vector == '=')
      #name <- paste(tolower(var_vector[1:(breaks[1] - 1)]), collapse = "_")
      cuml <-  as.numeric(var_vector[breaks[1] + 1])
      rate <-  as.numeric(var_vector[breaks[2] + 1])
      return(c(cuml,rate))
    }
    get_balance <- function(lines) {
      vars <- lapply(c((1:nr) + strt.in - 1, tot.in, (1:nr) + strt.out - 1, tot.out, inout, discrp),
                     function(i) read_vars(index = i, lines = lines))
      
      m <- do.call(cbind, c(get_timing(read_vars(index = 1, lines = lines)), 
                            lapply(vars, get_vars)))
      return(list(cuml = m[1,], rate = m[2,]))
    } 
    break_names <- function(var_vector) {
      breaks <- which(var_vector == '=')
      name <- paste(tolower(var_vector[1:(breaks[1] - 1)]), collapse = "_")
    }
    get_names <- function(lines) {
      vars <- lapply(c((1:nr) +strt.in - 1), function(i) read_vars(index = i, lines = lines))
      names <- unlist(lapply(vars, break_names))
      c('kstp', 'kper', paste(names, 'in', sep='_'), 'total_in',
        paste(names, 'out', sep='_'), 'total_out', 'difference', 'discrepancy')
    }
    
    # call  
    # set indices based on first budget
    lines <- lst.lines[headers[1]:enders[1]]
    strt.in <- 9
    tot.in <- grep('TOTAL IN', lines)
    end.in <- tot.in - 2
    strt.out <- tot.in + 4
    tot.out <- grep('TOTAL OUT', lines)
    end.out <- tot.out - 2
    inout <- tot.out + 2
    discrp <- inout + 2
    # number of variables; same in IN as in OUT
    nr <- (end.in - strt.in) + 1
    names <- get_names(lines)
    
    balance <- lapply(seq_along(headers), function(i) get_balance(lst.lines[headers[i]:enders[i]]))
    balance <-  list(cumulative = as.data.frame(do.call(rbind,lapply(balance, '[[', 'cuml'))), 
                     rates =     as.data.frame(do.call(rbind,lapply(balance, '[[', 'rate'))))
    colnames(balance$cumulative) <- colnames(balance$rates) <- names
    
    
    # no budget is printed
  } else {
    warning("No budget was printed to listing file. You can change this in the OC file.", call. = FALSE)
    return(NULL)
  }
  
  class(balance) <- 'bud'
  return(balance)
  
}


#' @rdname rmf_read_bud
#' @export
rmf_read_budget <- function(...) {
  rmf_read_bud(...)
}

#' Read a MODFLOW head predictions file
#' 
#' \code{rmf_read_hpr} reads in a MODFLOW head predictions file and returns it as an \code{\link{RMODFLOW}} hpr object.
#' 
#' @param file filename; typically '*.hpr'
#' @return object of class hpr
#' @export
rmf_read_hpr <- function(file = {cat('Please select hpr file ...\n'); file.choose()}) {
  
  hpr <- readr::read_table(file = file,
                           col_names = FALSE, skip = 1,
                           col_types = readr::cols(readr::col_double(), readr::col_double(), readr::col_character()))
  colnames(hpr) <- c('simulated', 'observed', 'name')
  hpr$residual <- hpr$simulated - hpr$observed
  hpr <- as.data.frame(hpr)
  attr(hpr, 'spec') <- NULL
  class(hpr) <- c('hpr', class(hpr))
  return(hpr)
}

#' @describeIn rmf_read_hpr Compatible with default ModelMuse file extension
#' @export
rmf_read_hob_out <- function(...) {
  rmf_read_hpr(...)
}
