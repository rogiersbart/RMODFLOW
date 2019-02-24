#' Read a MODFLOW cell-by-cell budget file
#' 
#' \code{rmf_read_cbc} reads in a MODFLOW cell-by-cell budget file
#' 
#' @param file filename; typically '*.cbc'
#' @param dis dis object.
#' @param huf huf object; optional. Provide only if huf heads are being read and \code{dis} is not NULL. See details.
#' @param oc oc object; optional. See details.
#' @param what character; denotes which flow terms to read. Defaults to reading all flow terms. See details.
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @return object of class cbc which is a list consisting of named rmf_arrays and/or data.frames. The names of the elements correspond to the flow terms.
#'
#' @details 
#' Flow terms include \code{'constant_head'}, \code{'storage'}, \code{'flow_right_face'}, \code{'flow_front_face'}, \code{'flow_lower_face'}, \code{'wells'},
#' \code{'river_leakage'}, \code{'recharge'}, \code{'drains'}, \code{'head_dep_bounds'} or any other description as written by MODFLOW.
#'  
#' If a \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If no \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY is read and binded at each time step for which output is written. 
#' The resulting dimensions of the final array are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#'
#' If flows are interpolated to huf units, a \code{huf} object is to be supplied as well to dimension the array. This will only affect the constant-head and cell flow terms.
#' The final array will have NHUF layers instead of NLAY.
#'
#' @importFrom readr read_lines
#' @export
rmf_read_cbc <- function(file = {cat('Please select cell-by-cell budget file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         huf = NULL,
                         oc = NULL,
                         precision = 'single',
                         what = 'all') {
  
  # headers <- c('   CONSTANT HEAD',
  #              '         STORAGE',
  #              'FLOW RIGHT FACE ',
  #              'FLOW FRONT FACE ',
  #              'FLOW LOWER FACE ',
  #              '           WELLS',
  #              '   RIVER LEAKAGE',
  #              '        RECHARGE',
  #              '          DRAINS',
  #              ' HEAD DEP BOUNDS')
  
  nbytes <- ifelse(precision == 'single', 4, 8) 
  binary = TRUE # MODFLOW cbc budget file is always binary
  if(binary) {
    con <- file(file,open='rb')
    cbc <- list()
    
    try({
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      desc <- readChar(con,nchars=16)
      trial <- 1
      fail = c(FALSE,FALSE)
      
      # nsteps to dimension array
      if(is.null(oc)) {
        nsteps <- sum(dis$nstp)
      } else {
        if(is.null(oc$save_budget)) {
          nsteps <- length(which(oc$icbcfl == T))
        } else {
         # problem: oc records might be in non-ascending order or have non-existing time steps but output is still writen for current timestep
         # e.g. UZFtest2
          m_oc<- cbind(oc$iperoc, oc$itsoc)[oc$save_budget,]
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
        if(trial == 1) {
          if(!(name %in% c("storage","constant_head",'flow_right_face','flow_front_face'))) {
            fail[1] = TRUE
          }
        } else if(trial == 2) {
          if(!(name %in% c('constant_head','flow_right_face','flow_front_face','flow_lower_face'))) {
            fail[2] = TRUE
          }
        }
        if(any(fail)) {
            stop(paste('Header descriptions do not match. Are you sure the file is', precision,'precision?'))
        }
        
        read <- ifelse((what != 'all' && !(name %in% what)), FALSE, TRUE) 
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
            attr(cbc[[name]], 'kstp') <- attr(cbc[[name]], 'kper') <- attr(cbc[[name]], 'pertim') <- attr(cbc[[name]], 'totim') <- attr(cbc[[name]], 'delt') <- rep(NA, nsteps)
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
                ctmp[nr] <- readChar(con,nchars=16)
              }
            }
            
            # return a data.frame --> might change to list of rmf_lists for more consistency with e.g. plotting
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
                  cbc[[name]] <- as.data.frame(cbind(ijk$k,ijk$i,ijk$j,df[,-1], nstp, kper, kstp))
                  names(cbc[[name]]) <- c('k','i','j', 'flow', if(nval > 1){ctmp},'nstp', 'kper','kstp')
                  rm(df)
                } else {
                  nstp <- ifelse(is.null(dis), cbc[[name]][nrow(cbc[[name]]),nstp]+1, stp_nr)
                  df <- as.data.frame(cbind(ijk$k,ijk$i,ijk$j,df[,-1], nstp, kper, kstp))
                  names(df) <- c('k','i','j', 'flow', if(nval > 1){ctmp},'nstp', 'kper', 'kstp')
                  
                  cbc[[name]] <- rbind(cbc[[name]], df)
                }
              } else {
                 if(is.array(cbc[[name]])) cbc[[name]] = NULL
              }
            }
            
            if(itype %in% c(0,1)) {
              cbc[[name]][,,,stp_nr] <- aperm(array(readBin(con,what='numeric',n=dis$ncol*dis$nrow*nnlay,size = nbytes),dim=c(dis$ncol,dis$nrow,nnlay)),c(2,1,3))
            }
            if(itype ==3) {
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
    class(cbc) <- c('cbc','rmf_package')
    return(cbc)
    
  } else {
    stop('Code not up to date')
    #     # update this to match the above structure!
    #     cbc <- list()
    #     cbc.lines <- read_lines(file)
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
    #     class(cbc) <- c('cbc','rmf_package')
    #     return(cbc)
  }
}
