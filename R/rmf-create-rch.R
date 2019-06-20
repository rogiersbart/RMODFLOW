#' Create an \code{RMODFLOW} rch object.
#' 
#' \code{rmf_create_rch} creates an \code{RMODFLOW} rch object
#' 
#' @param ... \code{rmf_2d_array's} (possibly of class \code{rmf_parameter}) or a single \code{list} with \code{rmf_2d_array's} (possibly of class \code{rmf_parameter}) elements; defines the recharge values. See details.
#' @param dis \code{RMODFLOW} dis object
#' @param nrchop recharge option code; defaults to 3 (recharge is applied to the highest active cell in each vertical column)
#' @param irchcb flag and unit number for writing cell-by-cell flow terms; defaults to 0 
#' @param irch list of \code{rmf_2d_array's} specifying the layer numbers defining in which layer recharge is applied. The \code{'kper'} attribute of the arrays define the stress period in which the array is active, see details. Only used when \code{nrchop = 2}. Defaults to NULL
#' @param irchpf numeric of length 1 or length \code{dis$nper}; optional format code for printing the \code{RECH} variable it has been defined by parameters; defaults to -1 (no printing) for all stress periods
#' @details the \code{rmf_2d_array's} should have \code{kper} attributes specifying the stress period in which they are active. This is also true for the irch arrays.
#' @return \code{RMODFLOW} rch object
#' @export
#' @seealso \code{\link{rmf_read_rch}}, \code{\link{rmf_write_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}

rmf_create_rch <- function(...,
                           dis,
                           nrchop = 3,
                           irchcb = 0,
                           irch = NULL,
                           irchpf = -1
) {
  
  arg <- rmfi_create_bc_array(arg = list(...), dis = dis)
  
  # create rch object
  obj <- list()
  
  obj$dimensions <- arg$dimensions
  obj$nrchop <- nrchop
  obj$irchcb <- irchcb
  obj$recharge <- arg$data
  if(arg$dimensions['np'] > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  if(nrchop == 2) {
    if(is.null(irch)) stop('Please supply a irch argument when nrchop = 2')
    if(!inherits(irch, 'list')) irch <- list(irch)
    obj$irch <- irch
    names(obj$irch) <- paste('irch', length(irch), sep = '_')
    obj$irch_kper <- data.frame(kper = 1:dis$nper)
    for(i in 1:length(irch)) {
      obj$irch_kper[[paste('irch', i, sep = '_')]] <- c(1:dis$nper) %in% attr(irch[[i]],'kper')
    }
    # check if multiple irch arrays are active
    irch_err <- vapply(1:dis$nper, function(i) sum(is.na(irch_kper[i,-1]) | irch_kper[i,-1] == TRUE) > 1, TRUE)
    if(any(irch_err)) stop(paste('There can be only 1 active irch array per stress period. Stress period(s)', which(irch_err), 'have multiple active arrays.'))
  } 
  obj$irchpf <- irchpf
  
  class(obj) <- c('rch', 'rmf_package')
  return(obj)

}
