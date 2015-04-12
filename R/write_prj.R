#' Write a projection file
#' 
#' \code{write.prj} writes a projection file
#' 
#' @param prj prj object
#' @param file Filename; typically *.prj
#' @export
write_prj <- function(prj,file)
{
  cat(paste0(prj$projection,'\n'), file=file)
  cat(paste0(paste0(prj$origin,collapse=' '),'\n'), file=file, append=TRUE)
  cat(paste0(prj$rotation,'\n'), file=file, append=TRUE)
}