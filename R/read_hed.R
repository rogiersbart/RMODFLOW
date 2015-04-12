#' Read a MODFLOW head file
#' 
#' \code{read_hed} reads in a MODFLOW head file and returns it as an \code{\link{RMODFLOW}} hed object.
#' 
#' @param file Filename; typically "*.hed"
#' @return Object of class hed
#' @export
read_hed <- function(file,  dis=read_dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')), ba6=read_ba6(paste(substring(file,1,nchar(file)-4),'.ba6',sep='')))
{
  hed <- NULL
  hed.lines <- scan(file, what=character(), sep='\n')
  hed <- dis$BOTM * NA
  for(k in 1:dis$NLAY)
  {
    # remove first line
    hed.lines <- hed.lines[-1] 
    
    # read heads
    hedVector <- NULL
    hedVector <- as.numeric(split_line_numbers(paste(hed.lines[1:(ceiling(dis$NCOL/10)*dis$NROW)],collapse=' ')))
    hed.lines <- hed.lines[-c(1:(ceiling(dis$NCOL/10)*dis$NROW))]
    hed[,,k] <- matrix(hedVector,nrow=dis$NROW,ncol=dis$NCOL,byrow=T)
  }
  #hed[which(hed==ba6$HNOFLO)] <- NA
  class(hed) <- c('hed','modflow_3d_array')
  return(hed)
}