#' Get an array specified by a control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param remaining_lines lines to read the array from
#' @param nrow number of rows in the array
#' @param ncol number of columns in the array
#' @param nlay number of layers in the array that should be read
#' @param ndim optional; dimensions of the array to read
#' @param skip_header optional; should the control record be skipped
#' @param nam a \code{RMODFLOW} nam object. Required when reading fixed-format or EXTERNAl arrays
#' @param precision character: either \code{'single'} (default) or \code{'double'}. Denotes the precision of binary files
#' @param file pathname to the MODFLOW input file which is currently being read. Required when reading fixed-format or OPEN/CLOSE arrays
#' @param integer logical; does the binary array hold integer values. Might not work optimally.
#' @param ... ignored
#' @return A list containing the array and the remaining text of the MODFLOW input file
#' @importFrom readr read_lines
#' @keywords internal
rmfi_parse_array <- function(remaining_lines,nrow,ncol,nlay, ndim = NULL,
                             skip_header = FALSE, nam = NULL, precision = "single", file = NULL, integer = FALSE, ...) {
  
  # Initialize array object
  array <- array(dim=c(nrow,ncol,nlay))
  fortranfmt <-  FALSE
  
  # Read array according to format type if there is anything to be read
  if(prod(dim(array))!=0)
  {
    for(k in 1:nlay) 
    { 
      # CONSTANT
      if(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1] == 'CONSTANT') {
        if(nlay==1) {
          array[1:length(array)] <- as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])[2])
          remaining_lines <- remaining_lines[-1]
        } else {
          array[,,k] <- matrix(as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])[2]),nrow=nrow,ncol=ncol)
          remaining_lines <- remaining_lines[-1]
        }
      }
      # INTERNAL or without header
      else if(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1] %in% c('INTERNAL') | skip_header)
      {
        
        if(!skip_header) {
          cnst <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2])
          if(cnst == 0) cnst <-  1.0
          
          # format
          fmtin <- rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[3]
          if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
             fmtin <- strsplit(fmtin, split='g|G|i|I|f|F|e|E|ES|es|EN|en')[[1]]
             fmtin <- as.numeric(strsplit(fmtin[length(fmtin)], split='\\.|)')[[1]][1])
             fmtin <- paste0(".{",fmtin,'}')
             fortranfmt <-  TRUE
          }
          remaining_lines <- remaining_lines[-1] 
        } else {
          cnst <-  1.0
        }
        if(fortranfmt) remaining_lines[1] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',remaining_lines[1])
        nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])))
        nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
        if(fortranfmt && nLines > 1) remaining_lines[2:nLines] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',remaining_lines[2:nLines])
        array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }
      # EXTERNAL
      else if(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='EXTERNAL')
      {
        nunit <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2])
        cnst <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[3])
        if(cnst == 0) cnst <-  1.0
        fmtin <-  as.character(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[4])
        binary <- ifelse(toupper(fmtin) == "(BINARY)", TRUE, FALSE)
    
        if(is.null(nam)) stop('Please supply a nam object when reading EXTERNAL arrays')
        fname <-  nam$fname[which(nam$nunit == nunit)]
        direct <-  attr(nam, 'dir')
        absfile = paste(direct, fname, sep = '/')
        
        if(!binary) {
          
          if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
            fmtin <- strsplit(fmtin, split='g|G|i|I|f|F|e|E|ES|es|EN|en')[[1]]
            fmtin <- as.numeric(strsplit(fmtin[length(fmtin)], split='\\.|)')[[1]][1])
            fmtin <- paste0(".{",fmtin,'}')
            fortranfmt <-  TRUE
          }
          
          # if external file holds multiple arrays, remove the corresponding lines
          external_lines <-  readr::read_lines(absfile)
          if(!is.null(attr(nam, as.character(nunit)))) external_lines <- external_lines[-c(1:attr(nam, as.character(nunit)))]
          
          if(fortranfmt) external_lines[1] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',external_lines[1])
          nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(external_lines[1],' |\t|,')[[1]])))
          nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
          if(fortranfmt && nLines > 1) external_lines[2:nLines] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',external_lines[2:nLines])
          array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          
        } else {
          con <- file(absfile,open='rb')
          type <- ifelse(integer, 'integer', 'numeric')
          if(type=='integer') warning('Reading integer binary EXTERNAL array might not work optimally')
          real_number_bytes <- ifelse(precision == 'single', 4, 8)
          size <- ifelse(type == 'integer', NA_integer_, real_number_bytes)
          try({ 
          
          # if external file holds multiple arrays, remove the corresponding lines
          if(!is.null(attr(nam, as.character(nunit)))) {
            for(jj in 1:attr(nam, as.character(nunit))) {
              invisible(readBin(con, what = 'integer', n = 2))
              invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
              invisible(readChar(con,nchars=16))
              nncol <- readBin(con, what = 'integer', n = 1)
              nnrow <- readBin(con, what = 'integer', n = 1)
              invisible(readBin(con, what = 'integer', n = 1))
              invisible(readBin(con,what='numeric',n = nncol * nnrow, size = real_number_bytes))
            }
          }
          # integer binary arrays should not have headers in MODFLOW (2005, v1.12 - see U2DINT subroutine, line 682)
          if(!integer) {
            invisible(readBin(con, what = 'integer', n = 2))
            invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
            invisible(readChar(con,nchars=16))
            invisible(readBin(con, what = 'integer', n = 3))
          }

          array[,,k] <- cnst*aperm(array(readBin(con,what=type,n = ncol * nrow, size = size),dim=c(ncol, nrow)), c(2, 1))
          nLines <-  1})
         
          close(con)
        }
        if(is.null(attr(nam, as.character(nunit)))) {
          attr(nam, as.character(nunit)) <- nLines
        } else {
          attr(nam, as.character(nunit)) <- attr(nam, as.character(nunit)) + nLines
        }
        remaining_lines <- remaining_lines[-1] 
      } 
      # OPEN/CLOSE
      else if(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='OPEN/CLOSE')
      {
        fname <-  as.character(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2])
        cnst <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[3])
        if(cnst == 0) cnst <-  1.0
        fmtin <-  as.character(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[4])
        binary <- ifelse(toupper(fmtin) == "(BINARY)", TRUE, FALSE)
        
        direct <-  dirname(file)
        absfile = paste(direct, fname, sep = '/')

        if(!binary) {
          if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
            fmtin <- strsplit(fmtin, split='g|G|i|I|f|F|e|E|ES|es|EN|en')[[1]]
            fmtin <- as.numeric(strsplit(fmtin[length(fmtin)], split='\\.|)')[[1]][1])
            fmtin <- paste0(".{",fmtin,'}')
            fortranfmt <-  TRUE
          }
          external_lines <-  readr::read_lines(absfile)

          if(fortranfmt) external_lines[1] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',external_lines[1])
          nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(external_lines[1],' |\t|,')[[1]])))
          nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
          if(fortranfmt && nLines > 1) external_lines[2:nLines] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',external_lines[2:nLines])
          array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          
        } else {
          con <- file(asbfile,open='rb')
          real_number_bytes <- ifelse(precision == 'single', 4, 8)
          type <- ifelse(integer, 'integer', 'numeric')
          size <- ifelse(type == 'integer', NA_integer_, real_number_bytes)
          try({     
            # integer binary arrays should not have headers in MODFLOW (2005, v1.12 - see U2DINT subroutine, line 682)
            if(!integer) { 
            invisible(readBin(con, what = 'integer', n = 2))
            invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
            invisible(readChar(con,nchars=16))
            invisible(readBin(con, what = 'integer', n = 3))
            }
            array[,,k] <- cnst*aperm(array(readBin(con,what=type,n = ncol * nrow, size = size),dim=c(ncol, nrow)), c(2, 1))
          })
          close(con)
        }
        remaining_lines <- remaining_lines[-1] 

      } else {
        # FIXED format
        if(is.null(nam)) stop('Please supply a nam object when reading FIXED-FORMAT arrays')
        locat <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1])
        cnst <- as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2])
        if(cnst == 0) cnst <-  1.0
        fmtin <-  as.character(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[3])
        
        # CONSTANT
        if(locat == 0) { 
          array[,,k] <- matrix(cnst, nrow=nrow, ncol=ncol)
        } else {
          
          fname <-  nam$fname[which(nam$nunit == locat)]
          direct <-  attr(nam, 'dir')
          absfile = paste(direct, fname, sep='/')
          
          # ASCII
          if(locat > 0) {
            if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
              fmtin <- strsplit(fmtin, split='g|G|i|I|f|F|e|E|ES|es|EN|en')[[1]]
              fmtin <- as.numeric(strsplit(fmtin[length(fmtin)], split='\\.|)')[[1]][1])
              fmtin <- paste0(".{",fmtin,'}')
              fortranfmt <-  TRUE
            }
            if(locat == nam$nunit[which(basename(nam$fname) == basename(file))]) { # read from current file
              
              remaining_lines <- remaining_lines[-1] 
              if(fortranfmt) remaining_lines[1] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',remaining_lines[1])
              nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])))
              nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
              if(fortranfmt && nLines > 1) remaining_lines[2:nLines] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',remaining_lines[2:nLines])
              array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
              
            } else { # read from external file
              external_lines <-  readr::read_lines(absfile)
              # remove lines of previous arrays
              if(!is.null(attr(nam, as.character(nunit)))) external_lines <- external_lines[-c(1:attr(nam, as.character(nunit)))]
              
              if(fortranfmt) external_lines[1] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',external_lines[1])
              nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(external_lines[1],' |\t|,')[[1]])))
              nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
              if(fortranfmt && nLines > 1) external_lines[2:nLines] <- gsub(paste0('(',fmtin,'?)'),'\\1\\ ',external_lines[2:nLines])
              array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
            }

          } else if(locat < 0) { # read binary from external file
            con <- file(asbfile,open='rb')
            real_number_bytes <- ifelse(precision == 'single', 4, 8)
            type <- ifelse(integer, 'integer', 'numeric')
            size <- ifelse(type == 'integer', NA_integer_, real_number_bytes)
            if(type=='integer') warning('Reading integer binary EXTERNAL array might not work optimally')
            
            try({          
              if(!is.null(attr(nam, as.character(nunit)))) {
               for(jj in 1:attr(nam, as.character(nunit))) {
                invisible(readBin(con, what = 'integer', n = 2))
                invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
                invisible(readChar(con,nchars=16))
                nncol <- readBin(con, what = 'integer', n = 1)
                nnrow <- readBin(con, what = 'integer', n = 1)
                invisible(readBin(con, what = 'integer', n = 1))
                invisible(readBin(con,what='numeric',n = nncol * nnrow, size = real_number_bytes))
               }
              }
              # integer binary arrays should not have headers in MODFLOW (2005, v1.12 - see U2DINT subroutine, line 682)
              if(!integer) {
                invisible(readBin(con, what = 'integer', n = 2))
                invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
                invisible(readChar(con,nchars=16))
                invisible(readBin(con, what = 'integer', n = 3))
              }
              
              array[,,k] <- cnst*aperm(array(readBin(con,what=type,n = ncol * nrow, size = size),dim=c(ncol, nrow)), c(2, 1))
              nLines <-  1})
  
            close(con)
          }
          if(is.null(attr(nam, as.character(nunit)))) {
            attr(nam, as.character(nunit)) <- nLines
          } else {
            attr(nam, as.character(nunit)) <- attr(nam, as.character(nunit)) + nLines
          }
        }
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }   
    }
  }
  
  # Set class of object (2darray; 3darray)
  if(is.null(ndim)) {
    if(nlay==1){
      if(ncol==1 || nrow==1) {
        array <- c(array(array,dim=nrow*ncol*nlay))
      } else {
        array <- as.matrix(array[,,1])
        class(array) <- 'rmf_2d_array'   
      }
    }
    if(nlay > 1) class(array) <- 'rmf_3d_array'
  } else if(ndim == 1) {
    array <- c(array(array,dim=nrow*ncol*nlay))
  } else if(ndim == 2) {
    array <- matrix(array[,,1], nrow = nrow, ncol = ncol)
    class(array) <- 'rmf_2d_array'     
  } else if(ndim == 3) {
    class(array) <- 'rmf_3d_array'
  }
  
  # Return output of reading function 
  return(list(array=array,remaining_lines=remaining_lines))
}
