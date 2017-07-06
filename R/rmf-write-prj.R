#' Write an RMODFLOW projection file
#' 
#' \code{write.prj} writes a projection file
#' 
#' @param prj an \code{\link{RMODFLOW}} prj object
#' @param file filename to write to; typically '*.prj'
#' @export
rmf_write_prj <- function(prj,
                      file = {cat('Please select prj file to overwrite or provide new filename ...\n'); file.choose()}) {
  cat(paste0(prj$projection,'\n'), file=file)
  cat(paste0(paste0(prj$origin,collapse=' '),'\n'), file=file, append=TRUE)
  cat(paste0(prj$rotation,'\n'), file=file, append=TRUE)
  if(length(prj) > 3) cat(paste0(format(prj$starttime,format='%Y-%m-%d %H:%M:%S'),'\n'), file=file, append=TRUE)
}

#' @describeIn rmf_write_prj Deprecated function name
write_prj <- function(...) {
  .Deprecated(new = "rmf_write_prj", old = "write_prj")
  rmf_write_prj(...)
}
