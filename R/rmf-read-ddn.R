#' Read a MODFLOW drawdown file
#' 
#' \code{rmf_read_ddn} reads in a MODFLOW drawdown file and returns it as an \code{RMODFLOW} ddn object.
#' 
#' @param file filename; typically '*.ddn'
#' @param dis dis object
#' @param oc oc object; optional. See details.
#' @param bas bas object; optional. If supplied, is used to set the hnoflo values to NA.
#' @param binary logical; is the file binary?
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @return object or list with objects of class ddn and rmf_4d_array. See details.
#' @details 
#' 
#' If no \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If a \code{oc} object is supplied, the dimensions of the returned array are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#' If the array is in ASCII format and no headers are present, a \code{OC} object must be supplied. In that case, it is assumed that the file being read only contains head values and the keyword XSECTION in the bas file is not present.
#' 
#' The only use of the bas argument is to replace the hnoflo values in the final array with NA's. 
#'
#' @importFrom readr read_lines abind abind
#' @export
rmf_read_ddn <- function(file = {cat('Please select ddn file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
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
      
      # if(!is.null(huf) && huf$iohufheads > 0) {
      #   dis$nlay <- huf$nhuf
      # }
      
      # check time steps if oc is specified
      if(!is.null(oc)) {
        # oc using words
        if(!is.null(oc$save_drawdown) ) {
          nsteps <- 1:length(which(oc$save_drawdown == TRUE))
          # oc using codes
        } else {
          nsteps <- 1:length(which(apply(oc$ddsv,2,function(i) any(i==TRUE))==TRUE))
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
        if(!is.null(oc$save_drawdown)) {
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
          
          # oc codes
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
      
      # skip non-drawdown arrays
      if(any(grepl('DRAWDOWN', hed.lines))) {
        hed.lines <-  hed.lines[grep('DRAWDOWN', hed.lines)[1]:length(hed.lines)]
      } else {
        other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
        hed.lines <-  NULL
      }
      
    }
  }
  
  
  if(!is.null(other_desc) && length(other_desc) != 0) {
    warning(paste('DRAWDOWN not found in file. Found ', length(other_desc), ' other descriptions'), call. = FALSE)
    warning(other_desc, call. = FALSE)
    warning('Returning NULL', call. = FALSE)
    return(NULL)
  } else {
    if(!is.null(bas)) hed[which(hed == bas$hnoflo)] <-  NA
    class(hed) <- c('ddn','rmf_4d_array')
    return(hed)
  }
}

#' @describeIn rmf_read_ddn
#' @export
rmf_read_drawdown <- function(...) {
  rmf_read_ddn(...)
}

