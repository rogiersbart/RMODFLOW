
#' Write a MODFLOW array to a separate file. 
#'
#' \code{rmf_write_array} writes a MODFLOW array to an output file. Binary and ASCII formats are supported
#'
#' @param array a 2D, 3D or 4D \code{rmf_array} to write.
#' @param file filename to write to
#' @param append logical; should the array be appended to the file
#' @param binary logical; should the array be written to a binary file
#' @param header logical; should a MODFLOW style header be written for the array (see 'Details'). Defaults to TRUE if binary is TRUE and FALSE otherwise.
#' @param dis optional \code{RMODFLOW} dis object. Used when \code{KPER}, \code{PERTIM} and \code{TOTIM} in the header should be exact.
#' @param desc character of maximum 16 characters. Used to set the \code{desc} element in the header. Default to \code{'HEAD'}
#' @param precision character; either \code{'single'} or \code{'double'}. Denotes the precision of the binary file.
#' @param xsection logical; does the array represent a cross-section. See 'Details'.
#' 
#' @details the header file consists of the following elements:
#'  \code{KSTP}, \code{KPER},\code{PERTIM},\code{DESC},\code{NCOL}, \code{NROW}, \code{ILAY} and \code{FMTIN}
#'  (see \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?frequently_asked_questions.htm} for their respective meaning.)
#'  If the array is 2D or 3D, \code{KSTP}, \code{KPER},\code{PERTIM} are always set to 1; otherwise they are all equal to the index of the 4th dimension.
#'  
#'  The \code{DESC} element must be read but it not used by MODFLOW itself. Users may want to specify a different \code{DESC} value when using the array with postprocessing software.
#'  
#'  The \code{FMTIN} element is not written for binary files. For ASCII files, \code{RMODFLOW} sets it as \code{NCOL * F}. Note that the ASCII format
#'  is irrelevant when using free-format reading and writing.
#'  
#'  \code{xsection} can be set to TRUE if the array represents a cross-section, i.e. the ibound or strt array in the
#'   \code{bas} file. The user must make sure the array is of dimension NLAY * NCOL. The sole function of \code{xsection} is to 
#'   set the \desc(ILAY) argument to -1 which promts MODFLOW to write slightly different information to the listing file. 
#'   \code{xsection} does not affect simulation results (assuming the array dimensions are correct)
#'  
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_array}}

rmf_write_array = function(array, file, append = FALSE, binary = FALSE, header = ifelse(binary, TRUE, FALSE), dis=NULL, desc = 'HEAD', precision = 'single', xsection = FALSE) {
  
  if(binary) { # binary
    if(header && is.integer(array)) warning("MODFLOW does not read a header line for a binary integer array. Consider setting header to FALSE")
    write_binary = function() {
      real_number_bytes <- ifelse(precision == 'single', 4, 8)
      size <- ifelse(is.integer(array), NA_integer_, real_number_bytes)
      ncell = prod(dim(array))
      desc = format(toupper(desc), width = 16, justify='right')
      
      if(is.null(dim(array))) {    # scalar
        if(header) {
          writeBin(1L, con=con) # KSTP
          writeBin(1L, con=con) # KPER
          writeBin(1, con=con, size = real_number_bytes) # PERTIM
          writeBin(1, con=con, size = real_number_bytes) # TOTIM
          writeChar(desc, con=con, nchars = 16, eos = NULL) # DESC
          writeBin(1L, con=con) # NCOL
          writeBin(1L, con=con) # NROW
          rmfi_ifelse0(xsection, writeBin(-1L, con=con),writeBin(1L, con=con)) # ILAY
        }
        writeBin(as.vector(array), con = con, size = size)
        
      } else if(length(dim(array))==2) {    # 2D
        if(header) {
          writeBin(1L, con=con) # KSTP
          writeBin(1L, con=con) # KPER
          writeBin(1, con=con, size = real_number_bytes) # PERTIM
          writeBin(1, con=con, size = real_number_bytes) # TOTIM
          writeChar(desc, con=con, nchars = 16, eos = NULL) # DESC
          writeBin(as.integer(dim(array)[2]), con=con) # NCOL
          writeBin(as.integer(dim(array)[1]), con=con) # NROW
          rmfi_ifelse0(xsection, writeBin(-1L, con=con),writeBin(1L, con=con)) # ILAY
        }
        writeBin(as.vector(aperm(array, c(2,1))), con = con, size = size)
        
        
      } else if(length(dim(array))==3) {    # 3D
        for(k in 1:dim(array)[3]) {
          if(header) {
            writeBin(1L, con=con) # KSTP
            writeBin(1L, con=con) # KPER
            writeBin(1, con=con, size = real_number_bytes) # PERTIM
            writeBin(1, con=con, size = real_number_bytes) # TOTIM
            writeChar(desc, con=con, nchars = 16, eos = NULL) # DESC
            writeBin(as.integer(dim(array)[2]), con=con) # NCOL
            writeBin(as.integer(dim(array)[1]), con=con) # NROW
            rmfi_ifelse0(xsection, writeBin(-1L, con=con),writeBin(as.integer(k), con=con)) # ILAY
          }
          writeBin(as.vector(aperm(array[,,k], c(2,1))), con = con, size = size)
        }
        
      } else if(length(dim(array))==4) {    # 4D
        
        if(header && is.null(dis)) warning('No dis object supplied; writing simplified header lines.')
  
        for(l in 1:dim(array)[4]) {
          for(k in 1:dim(array)[3]) {
            if(header) {
              if(is.null(dis)) {
                kper <- l
                kstp <- l
                totim <- sum(1:l)
                pertim <- l
              } else {
                kper <-  findInterval(l, cumsum(dis$nstp), left.open = T) + 1
                kstp <-  rmfi_ifelse0(kper > 1, l - cumsum(dis$nstp[kper-1]), l)
                totim <-  rmf_time_steps(dis)$cumsum[l]
                pertim <-  totim - rmf_time_steps(dis)$cumsum[cumsum(nstp)[kper-1]]
              }

              writeBin(as.integer(kstp), con=con) # KSTP
              writeBin(as.integer(kper), con=con) # KPER
              writeBin(pertim, con=con, size = real_number_bytes) # PERTIM
              writeBin(totim, con=con, size = real_number_bytes) # TOTIM
              writeChar(desc, con=con, nchars = 16, eos = NULL) # DESC
              writeBin(as.integer(dim(array)[2]), con=con) # NCOL
              writeBin(as.integer(dim(array)[1]), con=con) # NROW
              rmfi_ifelse0(xsection, writeBin(-1L, con=con),writeBin(as.integer(k), con=con)) # ILAY
            }
            writeBin(as.vector(aperm(array[,,k,l], c(2,1))), con = con, size = size)
          }
        }
      }
    }
    if(append) {
      con <-  file(file, open='ab')
    } else {
      con <-  file(file, open='wb')
    }
    
    try(write_binary())
    close(con)  
    
  } else { # ascii
    if(!append) close(file(file, open='w')) 
    
    if(length(dim(array))==3) {    # 3D
      for(k in 1:dim(array)[3]) {
        if(header) rmfi_write_variables(1, 1, 1, 1, format(desc, width=16, justify='right'), ncol(array), nrow(array), ifelse(xsection,-1,k), paste0('(',ncol(array),'F)'), file=file)
        write.table(array[,,k], file=file, col.names = F, row.names = F, append = TRUE)
      }
    } else if(length(dim(array))==4) {    # 4D
      if(header && is.null(dis)) warning('No dis object supplied; writing simplified header lines.')
      
      for(l in 1:dim(array)[4]) {
        for(k in 1:dim(array)[3]) {
          if(header) {
            if(is.null(dis)) {
              kper <- l
              kstp <- l
              totim <- sum(1:l)
              pertim <- l
            } else {
              kper <-  findInterval(l, cumsum(dis$nstp), left.open = T) + 1
              kstp <-  rmfi_ifelse0(kper > 1, l - cumsum(dis$nstp[kper-1]), l)
              totim <-  rmf_time_steps(dis)$cumsum[l]
              pertim <-  totim - rmf_time_steps(dis)$cumsum[cumsum(nstp)[kper-1]]
            }
            rmfi_write_variables(kper, kstp, totim, pertim, format(desc, width=16, justify='right'), ncol(array), nrow(array), ifelse(xsection,-1,k), paste0('(',ncol(array),'F)'), file=file)
          }
          write.table(array[,,k,l], file=file, col.names = F, row.names = F, append = TRUE)
        }
      }
    } else {
      if(header) rmfi_write_variables(1, 1, 1, 1, format(desc, width=16, justify='right'), ncol(array), nrow(array), ifelse(xsection, -1,1), paste0('(',ncol(array),'F)'), file=file)
      write.table(array, file=file, col.names = F, row.names = F, append = TRUE)
    }
    
  }
}
