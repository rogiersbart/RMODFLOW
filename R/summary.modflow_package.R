#' Summary of modflow package object
#' 
#' @return \code{NULL}
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