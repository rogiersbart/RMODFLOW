#' Read a MODFLOW head file
#' 
#' \code{read_hed} reads in a MODFLOW head file and returns it as an \code{\link{RMODFLOW}} hed object.
#' 
#' @param file filename; typically '*.hed'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param ba6 basic file object; defaults to that with the same filename but with extension '.ba6'
#' @param convert_HNOFLO_to_NA logical; should HNOFLO values be converted to NA?
#' @return object of class hed
#' @importFrom readr read_lines
#' @export
read_hed <- function(file,  dis=read_dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')), ba6=read_ba6(paste(substring(file,1,nchar(file)-4),'.ba6',sep='')), convert_HNOFLO_to_NA=TRUE)
{
  hed.lines <- read_lines(file)
#   for(k in 1:dis$NLAY)
#   {
    # remove format line
#       hed.lines <- hed.lines[-1] 
    
    # read heads
      dataSet <- read_modflow_array(hed.lines,dis$NROW,dis$NCOL,dis$NLAY)
      hed <- dataSet$modflow_array
#       hedVector <- NULL
#       hedVector <- as.numeric(split_line_numbers(paste(hed.lines[1:(ceiling(dis$NCOL/10)*dis$NROW)],collapse=' ')))
#       hed.lines <- hed.lines[-c(1:(ceiling(dis$NCOL/10)*dis$NROW))]
#       hed[,,k] <- matrix(hedVector,nrow=dis$NROW,ncol=dis$NCOL,byrow=T)
#   }
  if(convert_HNOFLO_to_NA) hed[which(hed==ba6$HNOFLO)] <- NA
  class(hed) <- c('hed','modflow_3d_array')
  return(hed)
}
