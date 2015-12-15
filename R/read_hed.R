#' Read a MODFLOW head file
#' 
#' \code{read_hed} reads in a MODFLOW head file and returns it as an \code{\link{RMODFLOW}} hed object.
#' 
#' @param file filename; typically '*.hed'
#' @param dis discretization file object
#' @param bas basic file object
#' @param huf huf object; optional; provide only if huf heads are being read
#' @param convert_hnoflo_to_NA logical; should hnoflo values be converted to NA?
#' @return object of class hed
#' @importFrom readr read_lines
#' @export
read_hed <- function(file = {cat('Please select hed file...\n'); file.choose()},
                     dis = {cat('Please select dis file...\n'); read_dis(file.choose())},
                     bas = {cat('Please select bas file...\n'); read_bas(file.choose(), dis = dis)},
                     huf = NULL,
                     convert_hnoflo_to_NA=TRUE,
                     binary = TRUE) {
  if(binary) {
    if(!is.null(huf)) {
      dis$nlay <- huf$nhuf
    }
    con <- file(file,open='rb')
    hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, sum(dis$nstp)))
    attr(hed, 'kstp') <- attr(hed, 'kper') <- attr(hed, 'pertim') <- attr(hed, 'totim') <- attr(hed, 'desc') <- attr(hed, 'ncol') <- attr(hed, 'nrow') <- attr(hed, 'ilay') <- NULL
    
    kstp <- readBin(con,what='integer',n=1)
    kper <- readBin(con,what='integer',n=1)
    pertim <- readBin(con,what='numeric',n = 1, size = 4)
    totim <- readBin(con,what='numeric',n = 1, size = 4)
    desc <- readChar(con,nchars=16)
    while(length(desc != 0)) {
      ncol <- readBin(con, what = 'integer', n = 1)
      nrow <- readBin(con, what = 'integer', n = 1)
      ilay <- readBin(con, what = 'integer', n = 1)
      stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
      hed[,,ilay,stp_nr] <- aperm(array(readBin(con,what='numeric',n = ncol * nrow, size = 4),dim=c(ncol, nrow)), c(2, 1))
      attr(hed, 'kstp')[stp_nr] <- kstp
      attr(hed, 'kper')[stp_nr] <- kper
      attr(hed, 'pertim')[stp_nr] <- pertim
      attr(hed, 'totim')[stp_nr] <- totim
      attr(hed, 'desc')[stp_nr] <- desc
      attr(hed, 'ncol')[stp_nr] <- ncol
      attr(hed, 'nrow')[stp_nr] <- nrow
      attr(hed, 'ilay')[stp_nr] <- ilay
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      pertim <- readBin(con,what='numeric',n = 1, size = 4)
      totim <- readBin(con,what='numeric',n = 1, size = 4)
      desc <- readChar(con,nchars=16)
    }
    no_data <- which(is.na(attr(hed, 'kstp')))
    if(length(no_data) != 0) {
      hed <- hed[,,,-no_data]
      attr(hed, 'kstp') <- attr(hed, 'kstp')[-no_data]
      attr(hed, 'kper') <- attr(hed, 'kper')[-no_data]
      attr(hed, 'pertim') <- attr(hed, 'pertim')[-no_data]
      attr(hed, 'totim') <- attr(hed, 'totim')[-no_data]
      attr(hed, 'desc') <- attr(hed, 'desc')[-no_data]
      attr(hed, 'ncol') <- attr(hed, 'ncol')[-no_data]
      attr(hed, 'nrow') <- attr(hed, 'nrow')[-no_data]
      attr(hed, 'ilay') <- attr(hed, 'ilay')[-no_data]
    }
    close(con)
  } else {
    hed.lines <- read_lines(file)
    hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, sum(dis$nstp)))
    attr(hed, 'kstp') <- attr(hed, 'kper') <- attr(hed, 'pertim') <- attr(hed, 'totim') <- attr(hed, 'desc') <- attr(hed, 'ncol') <- attr(hed, 'nrow') <- attr(hed, 'ilay') <- NULL
    
    while(length(hed.lines) != 0) {
      variables <- remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
      kstp <- as.numeric(variables[1])
      kper <- as.numeric(variables[2])
      pertim <- as.numeric(variables[3])
      totim <- as.numeric(variables[4])
      desc <- variables[5]
      ncol <- as.numeric(variables[6])
      nrow <- as.numeric(variables[7])
      ilay <- as.numeric(variables[8])
      stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
      data_set <- read_array(hed.lines,nrow,ncol,1)
      hed[,,ilay,stp_nr] <- data_set$array
      hed.lines <- data_set$remaining_lines
      attr(hed, 'kstp')[stp_nr] <- kstp
      attr(hed, 'kper')[stp_nr] <- kper
      attr(hed, 'pertim')[stp_nr] <- pertim
      attr(hed, 'totim')[stp_nr] <- totim
      attr(hed, 'desc')[stp_nr] <- desc
      attr(hed, 'ncol')[stp_nr] <- ncol
      attr(hed, 'nrow')[stp_nr] <- nrow
      attr(hed, 'ilay')[stp_nr] <- ilay
    }
    no_data <- which(is.na(attr(hed, 'kstp')))
    if(length(no_data != 0)) {
      hed <- hed[,,,-no_data]
      attr(hed, 'kstp') <- attr(hed, 'kstp')[-no_data]
      attr(hed, 'kper') <- attr(hed, 'kper')[-no_data]
      attr(hed, 'pertim') <- attr(hed, 'pertim')[-no_data]
      attr(hed, 'totim') <- attr(hed, 'totim')[-no_data]
      attr(hed, 'desc') <- attr(hed, 'desc')[-no_data]
      attr(hed, 'ncol') <- attr(hed, 'ncol')[-no_data]
      attr(hed, 'nrow') <- attr(hed, 'nrow')[-no_data]
      attr(hed, 'ilay') <- attr(hed, 'ilay')[-no_data]
    }
  }
  class(hed) <- c('hed','4d_array')
  if(convert_hnoflo_to_NA) hed[which(hed==bas$hnoflo)] <- NA
  return(hed)
}
