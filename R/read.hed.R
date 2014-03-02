################################################################################
### read.hed ###################################################################
################################################################################

read.hed <- function(file,  dis=read.dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')), ba6=read.ba6(paste(substring(file,1,nchar(file)-4),'.ba6',sep='')))
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
    hedVector <- as.numeric(split.line.num(paste(hed.lines[1:ceiling(dis$NROW*dis$NCOL/10)],collapse=' ')))
    hed.lines <- hed.lines[-c(1:ceiling(dis$NROW*dis$NCOL/10))]
    hed[,,k] <- matrix(hedVector,nrow=180,ncol=250,byrow=T)
  }
  #hed[which(hed==ba6$HNOFLO)] <- NA
  return(hed)
}

# # Old version
# read.hed <- function(file,  dis=read.dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')), ba6=read.ba6(paste(substring(file,1,nchar(file)-4),'.ba6',sep='')))
# {
#   hed <- NULL
#   hed.lines <- scan(file, what=character(), sep='\n')
#   hed <- dis$BOTM * NA
#   
#   for(k in 1:dis$NLAY)
#   {
#     # remove first line
#     hed.lines <- hed.lines[-1] 
#     
#     # read heads
#     hedVector <- NULL
#     for(i in 1:ceiling(dis$NROW*dis$NCOL/10))
#     {
#       hedVector <- append(hedVector,as.numeric(split.line.num(hed.lines[1])))
#       hed.lines <- hed.lines[-1]
#     }
#     for(i in 1:dis$NROW) for(j in 1:dis$NCOL) hed[i,j,k] <- hedVector[(i-1)*dis$NCOL+j]
#   }
#   hed[which(hed==ba6$HNOFLO)] <- NA
#   return(hed)
# }