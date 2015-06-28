#' Summary of modflow package object
#' 
#' @param modflow_package RMODFLOW modflow package object
#' @details
#' Provides the object comment attribute additional to the standard \code{summary} output.
#'
#' @rdname summary
#' @method summary modflow_package
#' @export
summary.modflow_package <- function(modflow_package)
{
  if(!is.null(comment(modflow_package)))
  {
    cat(paste(comment(modflow_package),collapse='\n'))
    cat('\n\n')
  }
  summary(unclass(modflow_package))
}