#' Structure of modflow package object
#' 
#' @return \code{NULL}
#'
#' @rdname str
#' @method str modflow_package
#' @export
str.modflow_package <- function(modflow_package)
{
  if(!is.null(comment(modflow_package)))
  {
    cat(paste(comment(modflow_package),collapse='\n'))
    cat('\n\n')
  }
  cat(paste(comment(modflow_package),sep='\n'))
  str(unclass(modflow_package))
}