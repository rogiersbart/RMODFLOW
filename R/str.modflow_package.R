#' Structure of modflow package object
#' 
#' @param modflow_package RMODFLOW modflow package object
#' @details
#' Provides the object comment attribute additional to the standard \code{str} output.
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
