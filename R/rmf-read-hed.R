#' Read a MODFLOW head file
#' 
#' \code{rmf_read_hed} reads in a MODFLOW head file and returns it as an \code{RMODFLOW} hed object.
#' 
#' @param file filename; typically '*.hed'
#' @param dis dis object
#' @param huf huf object; optional. Provide only if huf heads are being read. See details.
#' @param oc oc object; optional. See details.
#' @param bas bas object; optional. If supplied, is used to set the hnoflo values to NA.
#' @param binary logical; is the file binary?
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @return object of class hed and rmf_4d_array. See details.
#' @details 
#' When huf heads are to be read, a \code{huf} object should also be supplied. The final array will have NHUF layers instead of NLAY.
#' 
#' If no \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If a \code{oc} object is supplied, the dimensions of the returned array are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#' If the array is in ASCII format and no headers are present, a \code{OC} object must be supplied. In that case, it is assumed that the file being read only contains head values and the keyword XSECTION in the bas file is not present.
#' 
#' The only use of the bas argument is to replace the hnoflo values in the final array with NA's. 
#'
#' @importFrom readr read_lines 
#' @export
rmf_read_hed <- function(file = {cat('Please select head file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          huf = NULL,
                          oc = NULL,
                          bas = NULL,
                          binary = TRUE,
                          precision = 'single') {
  
  headers <- c('HEAD',
               'DRAWDOWN',
               'SUBSIDENCE',
               'COMPACTION',
               'CRITICAL HEAD', # spaces! fix!
               'HEAD IN HGU',
               'NDSYS COMPACTION',
               'Z DISPLACEMENT',
               'D CRITICAL HEAD',
               'LAYER COMPACTION',
               'DSYS COMPACTION',
               'ND CRITICAL HEAD',
               'LAYER COMPACTION',
               'SYSTM COMPACTION',
               'PRECONSOL STRESS',
               'CHANGE IN PCSTRS',
               'EFFECTIVE STRESS',
               'CHANGE IN EFF-ST',
               'VOID RATIO',
               'THICKNESS',
               'CENTER ELEVATION',
               'GEOSTATIC STRESS',
               'CHANGE IN G-STRS')
  other_desc <- NULL
  
  if(binary) { # Binary
    
    real_number_bytes <- ifelse(precision == 'single', 4, 8)
    con <- file(file,open='rb')
    
    try({   
      
      if(!is.null(huf) && huf$iohufheads > 0) {
        dis$nlay <- huf$nhuf
      }
      
      # check time steps if oc is specified
      if(!is.null(oc)) {
        # oc using words
        if(!is.null(oc$save_head)) {
          # problem: oc records might be in non-ascending order or have non-existing time steps but output is still writen for current timestep
          # e.g. UZFtest2
          m_oc<- cbind(oc$iperoc, oc$itsoc)[rmfi_ifelse0(is.matrix(oc$save_head), apply(oc$save_head, 2, any), oc$save_head),] 
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
          nsteps <- length(which(apply(oc$hdsv,2,function(i) any(i==TRUE))==TRUE))
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
        stop('Array description not recognized. Is the file really binary? If so, you could try double precision. If not, set the binary argument to FALSE.')
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
          if(ilay < 0 && first) {
            dis$nrow <- dis$nlay
            dis$nlay <- 1
          }
          ilay <- abs(ilay)
          
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
          
          hed[,,ilay,stp_nr] <- aperm(array(readBin(con,what='numeric',n = ncol * nrow, size = real_number_bytes),dim=c(ncol, nrow)), c(2, 1))
          
          attr(hed,'kstp')[stp_nr] <- kstp
          attr(hed,'kper')[stp_nr] <- kper
          attr(hed,'pertim')[stp_nr] <- pertim
          attr(hed,'totim')[stp_nr] <- totim
          
          first <- FALSE
          kstp <- readBin(con,what='integer',n=1)
          kper <- readBin(con,what='integer',n=1)
          pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          desc <- trimws(readChar(con,nchars=16))
        }
      }
    })
    close(con)
    
  } else { # ASCII
    hed.lines <- readr::read_lines(file)
    label <- TRUE
    variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
    desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
    if(! desc %in% headers) {
      if(variables[2] != dis$ncol) { # weak test to check if there's a label
        stop('Array description not recognized. Are you sure the file is not binary ?')
      }
      label <- FALSE
      if(is.null(oc)) {
        stop('No label line detected. Please specify an OC object.')
      } else {
        warning('Assuming file being read only contains head values.')
      }
    }
    
    
    # skip non-head arrays
    if(label) {
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
        variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
        kstp <- as.numeric(variables[1])
        kper <- as.numeric(variables[2])
        pertim <- as.numeric(variables[3])
        totim <- as.numeric(variables[4])
        desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
        if(! desc %in% headers) {
          stop('Array description not recognized. Are you sure the file is not binary ?')
        }
        name <- gsub(' ', '_', tolower(trimws(desc)))
        
        ncol <- as.numeric(variables[length(variables)-3])
        nrow <- as.numeric(variables[length(variables)-2])
        ilay <- abs(as.numeric(variables[length(variables)-1]))
        hed.lines <- hed.lines[-1]
        
        # xsection - ilay never negative with huf. Only do this once.
        if(ilay < 0 && !is.null(dis) && first) {
          dis$nrow <- dis$nlay
          dis$nlay <- 1
        }
        ilay <- abs(ilay)
        
        # no label
      } else {
        
        # oc words
        if(!is.null(oc$save_head)) {
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
          
          # oc codes
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
      data_set <- rmfi_parse_array(hed.lines,dis$nrow,dis$ncol,1, skip_header = TRUE)
      
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
  }
  
  if(!is.null(bas)) hed[which(hed == bas$hnoflo)] <-  NA
  
  if(!is.null(other_desc) && length(other_desc) != 0) {
    warning(paste('HEAD or HEAD IN HGU not found in file. Found ', length(other_desc), ' other descriptions'), call. = FALSE)
    warning(other_desc, call. = FALSE)
    warning('Returning NULL', call. = FALSE)
    return(NULL)
  } else {
    class(hed) <- c('hed','rmf_4d_array')
    return(hed)
  }
}

#' @describeIn rmf_read_hed 
#' @export
rmf_read_head <- function(...) {
  rmf_read_hed(...)
}

#' @describeIn rmf_read_hed Deprecated function name
#' @export
read_hed <- function(...) {
  .Deprecated(new = "rmf_read_hed", old = "read_hed")
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
