#' Read a GMS 2D grid file
#' 
#' \code{read_gms_2d_grid} reads in a GMS 2D grid file and returns it as an \code{\link{RMODFLOW}} gms2dgrid object.
#' 
#' @param file filename
#' @return object of class gms2dgrid
#' @importFrom readr read_lines
#' @export
rmf_read_gms_2d_grid <- function(file = {cat('Please select gms 2d grid file ...\n'); file.choose()}) {
  grid2d <- list()
  grid2d.lines <- read_lines(file)
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
  
  class(grid2d) <- 'gms_2d_grid'
  return(grid2d)
}

#' @describeIn rmf_read_gms_2d_grid Deprecated function name
#' @export
rmf_read_gms_2d_grid <- function(...) {
  .Deprecated(new = "rmf_read_gms_2d_grid", old = "read_gms_2d_grid")
  rmf_read_gms_2d_grid(...)
}
