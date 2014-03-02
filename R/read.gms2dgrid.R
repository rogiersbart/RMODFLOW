#' Read a GMS 2D grid file
#' 
#' \code{read.gms2dgrid} reads in a GMS 2D grid file and returns it as an \code{\link{RMODFLOW}} gms2dgrid object.
#' 
#' @param file Filename
#' @return Object of class gms2dgrid
#' @export
read.gms2dgrid <- function(file)
{
  grid2d <- NULL
  grid2d.lines <- scan(file, what=character(), sep='\n')
  #2dgrid.lines <- remove.comments.from.lines(mlt.lines)
  grid2d.lines <- grid2d.lines[-1]    
  grid2d$objtype <- as.character(strsplit(grid2d.lines[1],'\"')[[1]][2])
  grid2d.lines <- grid2d.lines[-1]
  grid2d.lines <- grid2d.lines[-1]
  grid2d$nd <- as.numeric(strsplit(grid2d.lines[1],' ')[[1]][3])
  grid2d.lines <- grid2d.lines[-1]
  grid2d$nc <- as.numeric(strsplit(grid2d.lines[1],' ')[[1]][3])    
  grid2d.lines <- grid2d.lines[-1]
  grid2d$n <- as.character(strsplit(grid2d.lines[1],'\"')[[1]][2])    
  grid2d.lines <- grid2d.lines[-1]    
  grid2d.lines <- grid2d.lines[-1] 
  
  grid2d$nddata <- as.numeric(grid2d.lines[1:grid2d$nd])
  grid2d$ncdata <- as.numeric(grid2d.lines[(1+grid2d$nd):(grid2d$nd + grid2d$nc)])              
  
  class(grid2d) <- 'gms2dgrid'
  return(grid2d)
}