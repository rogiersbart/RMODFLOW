#' Read a MODFLOW array from a separate file. 
#'
#' \code{rmf_read_array} reads a MODFLOW array from a separate file. Binary and ASCII formats are supported
#' 
#' @param file filename to read the array from
#' @param nrow number of rows in the array
#' @param ncol number of columns in the array
#' @param nlay number of layers in the array that should be read (3th dimension); defaults to 1
#' @param nstp number of timesteps in the array that should be read (4th dimension); defaults to 1
#' @param binary logical; is the array read from a binary file.
#' @param integer logical; does the array hold integer values. Only used for binary files. Might not work optimally.
#' @param header logical; should a MODFLOW style header be read for the array (see 'Details'). Defaults to TRUE if binary is TRUE and FALSE otherwise.
#' @param precision character: either \code{'single'} (default) or \code{'double'}. Denotes the precision of the binary file.
#'
#' @details \code{nrow}, \code{ncol}, \code{nlay}, \code{nstp} have to be specified if header is FALSE. They are used to dimension the array.
#'  
#'  The \code{integer} flag is only used when reading binary files.
#'  
#'  The header file consists of the following elements:
#'  \code{KSTP}, \code{KPER},\code{PERTIM},\code{DESC},\code{NCOL}, \code{NROW}, \code{ILAY} and \code{FMTIN}
#'  (see \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?frequently_asked_questions.htm} for their respective meaning.)
#'  The \code{FMTIN} element is not read for binary files.
#'  If a header is read, the values are set as attributes to the array.
#' 
#' @return a rmf_array with optional attributes if a header was read.
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_array}}

rmf_read_array = function(file, nrow = NULL, ncol = NULL, nlay=1, nstp=1, binary = F, integer = F, header = ifelse(binary, TRUE, FALSE), precision = 'single') {
  
  if(!header) {
    if(is.null(nrow) || is.null(ncol) || is.null(nlay) || is.null(nstp)) {
      stop('Either provide nrow, ncol, nlay and nstp or set header to TRUE')
    }
  }
  
  if(binary) { # Binary
    
    read_binary = function() {
      real_number_bytes <- ifelse(precision == 'single', 4, 8)
      type <- ifelse(integer, 'integer', 'numeric')
      if(header) {

        kstp <- readBin(con,what='integer',n=1)
        kper <- readBin(con,what='integer',n=1)
        pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
        totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
        desc <- readChar(con,nchars=16)
        
        stp_nr <- 0
        
        while(length(desc != 0)) {

          ncol <- readBin(con, what = 'integer', n = 1)
          nrow <- readBin(con, what = 'integer', n = 1)
          ilay <- abs(readBin(con, what = 'integer', n = 1)) # abs for XSECTION
          
          if(stp_nr == 0) { # initialize 3d array
              arr <- aperm(array(readBin(con,what=type,n = ncol * nrow, size = ifelse(integer, NA_integer_, real_number_bytes)),dim=c(ncol, nrow, 1)), c(2, 1, 3))
              kstp_attr <- kper_attr <- pertim_attr <- totim_attr <- desc_attr <- ncol_attr <- nrow_attr <- ilay_attr <- NULL
          } else { # read (abind drops attributes)
              arr <- abind::abind(arr, 
                                  aperm(array(readBin(con,what=type,n = ncol * nrow, size = ifelse(integer, NA_integer_, real_number_bytes)),dim=c(ncol, nrow)), c(2, 1)),
                                  along = 3)
          }

          # stp_nr only increases after each layer loop
          if(ilay == 1) {
            stp_nr <- stp_nr+1
            kstp_attr[stp_nr] <- kstp
            kper_attr[stp_nr] <- kper
            pertim_attr[stp_nr] <- pertim
            totim_attr[stp_nr] <- totim
            desc_attr[stp_nr] <- desc
            ncol_attr[stp_nr] <- ncol
            nrow_attr[stp_nr] <- nrow
          }
          # outside if-statement; so it will be equal to nlay; similar to rmf_read_hed
            ilay_attr[stp_nr] <- ilay 
          
          kstp <- readBin(con,what='integer',n=1)
          kper <- readBin(con,what='integer',n=1)
          pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          desc <- readChar(con,nchars=16)
        }
 
      } else {
        arr <- array(NA, dim = c(nrow, ncol, nlay, nstp))
        
        for(stp_nr in 1:nstp) {
          for(ilay in 1:nlay) {
            arr[,,ilay,stp_nr] <- aperm(array(readBin(con,what=type,n = ncol * nrow, size = ifelse(integer, NA_integer_, real_number_bytes)),dim=c(ncol, nrow)), c(2, 1))
          }
        }
      } 
      return(arr)
    }
    con <- file(file,open='rb')
    arr = try(read_binary())
    close(con)
    
  } else { # ASCII
    lines <- readr::read_lines(file)

    if(header) {
      stp_nr <- 0
      
      while(length(lines) != 0) {
        variables <- rmfi_remove_empty_strings(strsplit(lines[1],' ')[[1]])
        kstp <- as.numeric(variables[1])
        kper <- as.numeric(variables[2])
        pertim <- as.numeric(variables[3])
        totim <- as.numeric(variables[4])
        desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
        
        ncol <- as.numeric(variables[length(variables)-3])
        nrow <- as.numeric(variables[length(variables)-2])
        ilay <- abs(as.numeric(variables[length(variables)-1]))
        lines <- lines[-1]
         
        data_set <- rmfi_parse_array(lines,nrow,ncol,1, skip_header = TRUE)
        
        if(stp_nr == 0) { # initialize 3d array
          arr <- array(data_set$array, dim = c(dim(data_set$array), 1))
          kstp_attr <- kper_attr <- pertim_attr <- totim_attr <- desc_attr <- ncol_attr <- nrow_attr <- ilay_attr <- NULL
        } else { # read (abind drops attributes)
          arr <- abind::abind(arr, data_set$array, along = 3)
        }
        lines <- data_set$remaining_lines
        
        # stp_nr only increases after each layer loop
        if(ilay == 1) {
          stp_nr <- stp_nr+1
          kstp_attr[stp_nr] <- kstp
          kper_attr[stp_nr] <- kper
          pertim_attr[stp_nr] <- pertim
          totim_attr[stp_nr] <- totim
          desc_attr[stp_nr] <- desc
          ncol_attr[stp_nr] <- ncol
          nrow_attr[stp_nr] <- nrow
        }
        # outside if-statement; so it will be equal to nlay; similar to rmf_read_hed
        ilay_attr[stp_nr] <- ilay 
      }
      
    } else {
      arr <- array(NA, dim = c(nrow, ncol, nlay, nstp))
      
      for(stp_nr in 1:nstp)  {
        for(ilay in 1:nlay) {
          data_set <- rmfi_parse_array(lines,nrow,ncol,1, skip_header = TRUE)
          arr[,,ilay,stp_nr] <- data_set$array
          lines <- data_set$remaining_lines
        }
      }
    }
    if(integer) arr <- apply(arr, MARGIN = 1:length(dim(arr)), function(i) as.integer(i))
  }
  
  if(header) {
    # create list for each time step; abind to 4d array
    if(stp_nr > 1) {
      arr <- abind::abind(lapply(seq(1,dim(arr)[3],ilay), function(i) rmfi_ifelse0(ilay==1,arr,arr[,,i:(i+ilay-1)])), along = 4)
    }else {
      arr <- array(arr, dim = c(dim(arr), 1))
    }
  }
  nrow <- dim(arr)[1]
  ncol <- dim(arr)[2]
  nlay <- dim(arr)[3]
  nstp <- dim(arr)[4]
  
  # Set class of object (2darray; 3darray; 4darray)
  if(length(which(c(nrow,ncol,nlay,nstp) !=1 )) <= 1) {
    arr <- c(array(arr,dim=nrow*ncol*nlay*nstp))
  } else if(nrow !=1 && ncol !=1 && nlay == 1 && nstp == 1) {
    arr <- arr[,,1,1]
    class(arr) <- 'rmf_2d_array'   
  } else if(nstp != 1) {
    class(arr) <- 'rmf_4d_array'
  } else {
    arr <- arr[,,,1]
    class(arr) <- 'rmf_3d_array'
  }
  
  if(header) {
    
    attr(arr, 'dimnames') <- NULL
    attr(arr, 'kstp') <- kstp_attr
    attr(arr, 'kper') <- kper_attr
    attr(arr, 'pertim') <- pertim_attr
    attr(arr, 'totim') <- totim_attr
    attr(arr, 'desc') <- desc_attr
    attr(arr, 'ncol') <- ncol_attr
    attr(arr, 'nrow') <- nrow_attr
    attr(arr, 'ilay') <- ilay_attr
    
    no_data <- which(is.na(attr(arr, 'kstp')))
    if(length(no_data) != 0) {
      arr <- arr[,,,-no_data]
      attr(arr, 'kstp') <- attr(arr, 'kstp')[-no_data]
      attr(arr, 'kper') <- attr(arr, 'kper')[-no_data]
      attr(arr, 'pertim') <- attr(arr, 'pertim')[-no_data]
      attr(arr, 'totim') <- attr(arr, 'totim')[-no_data]
      attr(arr, 'desc') <- attr(arr, 'desc')[-no_data]
      attr(arr, 'ncol') <- attr(arr, 'ncol')[-no_data]
      attr(arr, 'nrow') <- attr(arr, 'nrow')[-no_data]
      attr(arr, 'ilay') <- attr(arr, 'ilay')[-no_data]
    }
  }
  
  return(arr)
  
}

