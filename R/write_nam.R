#' Write a MODFLOW name file
#' 
#' \code{write_nam} writes a MODFLOW name file based on an \code{\link{RMODFLOW}} nam object.
#' 
#' @param nam an \code{\link{RMODFLOW}} nam object
#' @param file filename to write to; typically '*.nam'
#' @return \code{NULL}
#' @export
write_nam <- function(nam,
                      file = {cat('Please select nam file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 1
    write.table(nam, file = file, row.names = FALSE, col.names = FALSE, quote = FALSE)
}
