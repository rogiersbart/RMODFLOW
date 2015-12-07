#' Read a MODFLOW head file
#' 
#' \code{read_hed} reads in a MODFLOW head file and returns it as an \code{\link{RMODFLOW}} hed object.
#' 
#' @param file filename; typically '*.hed'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param bas basic file object; defaults to that with the same filename but with extension '.bas'
#' @param convert_HNOFLO_to_NA logical; should HNOFLO values be converted to NA?
#' @return object of class hed
#' @importFrom readr read_lines
#' @export
read_hed <- function(file = {cat('Please select hed file...\n'); file.choose()},
                     dis = {cat('Please select dis file...\n'); read_dis(file.choose())},
                     bas = {cat('Please select bas file...\n'); read_bas(file.choose(), dis = dis)},
                     convert_HNOFLO_to_NA=TRUE,
                     binary = TRUE) {
  if(binary) {
    con <- file(file,open='rb')
    hed <- array(NA, dim = c(dis$NROW, dis$NCOL, dis$NLAY, sum(dis$NSTP)))
    attr(hed, 'KSTP') <- attr(hed, 'KPER') <- attr(hed, 'PERTIM') <- attr(hed, 'TOTIM') <- attr(hed, 'DESC') <- attr(hed, 'NCOL') <- attr(hed, 'NROW') <- attr(hed, 'ILAY') <- NULL
    
    KSTP <- readBin(con,what='integer',n=1)
    KPER <- readBin(con,what='integer',n=1)
    PERTIM <- readBin(con,what='numeric',n = 1, size = 4)
    TOTIM <- readBin(con,what='numeric',n = 1, size = 4)
    DESC <- readChar(con,nchars=16)
    while(length(DESC != 0)) {
      NCOL <- readBin(con, what = 'integer', n = 1)
      NROW <- readBin(con, what = 'integer', n = 1)
      ILAY <- readBin(con, what = 'integer', n = 1)
      stp_nr <- ifelse(KPER==1,KSTP,cumsum(dis$NSTP)[KPER-1]+KSTP)
      hed[,,ILAY,stp_nr] <- aperm(array(readBin(con,what='numeric',n = NCOL * NROW, size = 4),dim=c(NCOL, NROW)), c(2, 1))
      attr(hed, 'KSTP')[stp_nr] <- KSTP
      attr(hed, 'KPER')[stp_nr] <- KPER
      attr(hed, 'PERTIM')[stp_nr] <- PERTIM
      attr(hed, 'TOTIM')[stp_nr] <- TOTIM
      attr(hed, 'DESC')[stp_nr] <- DESC
      attr(hed, 'NCOL')[stp_nr] <- NCOL
      attr(hed, 'NROW')[stp_nr] <- NROW
      attr(hed, 'ILAY')[stp_nr] <- ILAY
      KSTP <- readBin(con,what='integer',n=1)
      KPER <- readBin(con,what='integer',n=1)
      PERTIM <- readBin(con,what='numeric',n = 1, size = 4)
      TOTIM <- readBin(con,what='numeric',n = 1, size = 4)
      DESC <- readChar(con,nchars=16)
    }
    no_data <- which(is.na(attr(hed, 'KSTP')))
    hed <- hed[,,,-no_data]
    attr(hed, 'KSTP') <- attr(hed, 'KSTP')[-no_data]
    attr(hed, 'KPER') <- attr(hed, 'KPER')[-no_data]
    attr(hed, 'PERTIM') <- attr(hed, 'PERTIM')[-no_data]
    attr(hed, 'TOTIM') <- attr(hed, 'TOTIM')[-no_data]
    attr(hed, 'DESC') <- attr(hed, 'DESC')[-no_data]
    attr(hed, 'NCOL') <- attr(hed, 'NCOL')[-no_data]
    attr(hed, 'NROW') <- attr(hed, 'NROW')[-no_data]
    attr(hed, 'ILAY') <- attr(hed, 'ILAY')[-no_data]
  } else {
    hed.lines <- read_lines(file)
    hed <- array(NA, dim = c(dis$NROW, dis$NCOL, dis$NLAY, sum(dis$NSTP)))
    attr(hed, 'KSTP') <- attr(hed, 'KPER') <- attr(hed, 'PERTIM') <- attr(hed, 'TOTIM') <- attr(hed, 'DESC') <- attr(hed, 'NCOL') <- attr(hed, 'NROW') <- attr(hed, 'ILAY') <- NULL
    
    while(length(hed.lines) != 0) {
      variables <- remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
      KSTP <- as.numeric(variables[1])
      KPER <- as.numeric(variables[2])
      PERTIM <- as.numeric(variables[3])
      TOTIM <- as.numeric(variables[4])
      DESC <- variables[5]
      NCOL <- as.numeric(variables[6])
      NROW <- as.numeric(variables[7])
      ILAY <- as.numeric(variables[8])
      stp_nr <- ifelse(KPER==1,KSTP,cumsum(dis$NSTP)[KPER-1]+KSTP)
      dataSet <- read_array(hed.lines,NROW,NCOL,1)
      hed[,,ILAY,stp_nr] <- dataSet$modflow_array
      hed.lines <- dataSet$remaining_lines
      attr(hed, 'KSTP')[stp_nr] <- KSTP
      attr(hed, 'KPER')[stp_nr] <- KPER
      attr(hed, 'PERTIM')[stp_nr] <- PERTIM
      attr(hed, 'TOTIM')[stp_nr] <- TOTIM
      attr(hed, 'DESC')[stp_nr] <- DESC
      attr(hed, 'NCOL')[stp_nr] <- NCOL
      attr(hed, 'NROW')[stp_nr] <- NROW
      attr(hed, 'ILAY')[stp_nr] <- ILAY
    }
    no_data <- which(is.na(attr(hed, 'KSTP')))
    if(length(no_data != 0)) {
      hed <- hed[,,,-no_data]
      attr(hed, 'KSTP') <- attr(hed, 'KSTP')[-no_data]
      attr(hed, 'KPER') <- attr(hed, 'KPER')[-no_data]
      attr(hed, 'PERTIM') <- attr(hed, 'PERTIM')[-no_data]
      attr(hed, 'TOTIM') <- attr(hed, 'TOTIM')[-no_data]
      attr(hed, 'DESC') <- attr(hed, 'DESC')[-no_data]
      attr(hed, 'NCOL') <- attr(hed, 'NCOL')[-no_data]
      attr(hed, 'NROW') <- attr(hed, 'NROW')[-no_data]
      attr(hed, 'ILAY') <- attr(hed, 'ILAY')[-no_data]
    }
  }
  class(hed) <- c('hed','4d_array')
  if(convert_HNOFLO_to_NA) hed[which(hed==bas$HNOFLO)] <- NA
  return(hed)
}
