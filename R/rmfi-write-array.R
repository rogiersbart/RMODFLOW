#' Write modflow array
#' Internal function used in the write_* functions for writing array datasets
#' @param external character vector with names corresponding to the dataset; used to write external arrays
#' @param fname  character vector with names corresponding to the dataset; used to write open/close arrays
#' @param binary character vector with names corresponding to the dataset; used to write external or open/close arrays
#' @param precision character: either \code{'single'} (default) or \code{'double'}. Denotes the precision of binary files
#' @param nam \code{\link{RMODFLOW}} nam object; used when writing external arrays
#' @param ... ignored
rmfi_write_array <- function(array, file, cnstnt=1, iprn=-1, append=TRUE, external = NULL, fname = NULL, binary = NULL, precision = 'single', nam = NULL, xsection = FALSE, ...) {
  
  arrname <-  sub(x=sub(".*[$]","",deparse(substitute(array))),pattern = '[[].*', replacement='')
  external <- rmfi_ifelse0(is.null(external), FALSE, arrname %in% external)
  fname <- rmfi_ifelse0(is.null(fname), FALSE, arrname %in% fname)
  binary <- rmfi_ifelse0(is.null(binary), FALSE, arrname %in% binary)
  
  if(is.null(names(cnstnt))) {
    if(length(cnstnt) > 1)  stop('Please supply a single value or a named vector for cnstnt')
  } else {
    cnstnt <- ifelse(is.na(cnstnt[arrname]), 1, cnstnt[arrname])
  }
  
  if(is.null(names(iprn))) {
    if(length(iprn) > 1)  stop('Please supply a single value or a named vector for iprn')
  } else {
    iprn <- ifelse(is.na(iprn[arrname]), -1, iprn[arrname])
  }
  
  if(external) { # external
    if(is.null(nam)) stop('Please supply a nam object when writing EXTERNAL arrays')
    extfile <-  paste(dirname(file), paste(arrname, 'ext', sep='.'), sep='/')
    
    # set unit number in nam file
    found <-  F
    nunit <- 200
    while(!found) {
      if(!(nunit %in%nam$nunit)) {
        nam <-  rbind(nam, list(ifelse(binary[arrname],"DATA(BINARY)","DATA"),nunit,extfile,NA))
        found <- T
      } else {
        nunit <-  nunit+1
      }
    }
    
    if(!is.null(dim(array)) && length(dim(array)) > 2) {
      for(i in 1:dim(array)[3]) {
        cat(paste('EXTERNAL',nunit, cnstnt, ifelse(binary,"(binary)","(free)"), iprn, '\n', sep=' '), file=file, append=append)
      }
    } else {
      cat(paste('EXTERNAL',nunit, cnstnt, ifelse(binary,"(binary)","(free)"), iprn, '\n', sep=' '), file=file, append=append)
    }

    rmf_write_array(array = array, file = extfile, append = FALSE, binary = binary, header = ifelse(binary, ifelse(is.integer(array), FALSE,TRUE), FALSE), desc = 'HEAD', precision = precision, xsection = xsection)
    warning(paste('Remember to add the external file to the nam file.\nftype =', ifelse(binary,"DATA(BINARY)","DATA"),
                  '\nnunit =', nunit, '\nfname =', extfile))
    #return(data.frame(ftype = ifelse(binary[arrname], 'DATA(BINARY)', 'DATA'), nunit=nunit, fname=extfile, options=NA))
    
  } else if(fname) { # open/close
    
    if(!is.null(dim(array)) && length(dim(array)) > 2) {
      for(i in 1:dim(array)[3]) {
        fname_i <- paste(paste(arrname, i, sep = '_'), 'in', sep = '.')
        direct <-  dirname(file)
        absfile <-  paste(direct, fname_i, sep = '/')

        cat(paste('OPEN/CLOSE', fname_i, cnstnt, ifelse(binary,"(binary)","(free)"), iprn, '\n', sep=' '), file=file, append=append)
        rmf_write_array(array = array[,,i], file = absfile, append = FALSE, binary = binary, header = ifelse(binary, ifelse(is.integer(array), FALSE,TRUE), FALSE), desc = 'HEAD', precision = precision, xsection = xsection)
        
      }
    } else {
      fname_i <- paste(arrname, 'in', sep = '.')
      direct <-  dirname(file)
      absfile <-  paste(direct, fname_i, sep = '/')
      cat(paste('OPEN/CLOSE', fname_i, cnstnt, ifelse(binary,"(binary)","(free)"), iprn, '\n', sep=' '), file=file, append=append)
      rmf_write_array(array = array, file = absfile, append = FALSE, binary = binary, header = ifelse(binary, ifelse(is.integer(array), FALSE,TRUE), FALSE), desc = 'HEAD', precision = precision, xsection = xsection)
      
    }
    
    
  } else { # not external or open/close
    if(is.null(dim(array))) {
      if(prod(c(array)[1] == c(array))==1) {
        cat(paste('CONSTANT ',cnstnt * c(array)[1], '\n', sep=''), file=file, append=append)
      } else {
        cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
        cat(paste(paste(array, collapse=' '), '\n', sep=' '), file=file, append=append)     
      }
    } else if(length(dim(array))==2) {
      if(prod(c(array)[1] == c(array))==1) {
        cat(paste('CONSTANT ',cnstnt * c(array)[1], '\n', sep=''), file=file, append=append)
      } else {
        cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
        if(dim(array)[1] == 1) {
          cat(paste0(paste(array, collapse=' '),'\n'), file=file, append=append)
        } else {
          write.table(array, file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE) 
        }
      }
    } else {
      for(i in 1:dim(array)[3])
      {
        if(prod(c(array[,,i])[1] == c(array[,,i]))==1) {
          cat(paste('CONSTANT ',cnstnt * c(array[,,i])[1], '\n', sep=''), file=file, append=append)
        } else {
          cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
          if(dim(array)[1] == 1) {
            cat(paste0(paste(array[,,i], collapse=' '),'\n'), file=file, append=append)
          } else {
            write.table(array[,,i], file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE)       
          }
        }
      }
    }
  }
}
