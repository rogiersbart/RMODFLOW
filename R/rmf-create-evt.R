#' Create an \code{RMODFLOW} evt object.
#' 
#' \code{rmf_create_evt} creates an \code{RMODFLOW} evt object
#' 
#' @param ... \code{rmf_2d_array's} (possibly of class \code{rmf_parameter}) or a single \code{list} with \code{rmf_2d_array's} (possibly of class \code{rmf_parameter}) elements; defines the maximum evapotranspiration fluxes. See details.
#' @param dis \code{RMODFLOW} dis object
#' @param nevtop evapotranspiration (ET) option code; defaults to 3 (ET is applied to the highest active cell in each vertical column)
#' @param ievtcb flag and unit number for writing cell-by-cell flow terms; defaults to 0 
#' @param surf list of \code{rmf_2d_array's} specifying the elevation of the ET surface. The \code{'kper'} attribute of the arrays define the stress period in which the array is active, see details. At least 1 surf array must be supplied.
#' @param exdp list of \code{rmf_2d_array's} specifying the ET extinction depth as a distance from surf. The \code{'kper'} attribute of the arrays define the stress period in which the array is active, see details. At least 1 exdp array must be supplied.
#' @param ievt list of \code{rmf_2d_array's} specifying the layer numbers defining in which layer ET is applied. The \code{'kper'} attribute of the arrays define the stress period in which the array is active, see details. Only used when \code{nevtop = 2}. Defaults to NULL
#' @param ievtpf numeric of length 1 or length \code{dis$nper}; optional format code for printing the \code{ET} variable it has been defined by parameters; defaults to -1 (no printing) for all stress periods
#' @details the \code{rmf_2d_array's} should have \code{kper} attributes specifying the stress period in which they are active. This is also true for the surf, exdp and ievt arrays.
#' @return \code{RMODFLOW} evt object
#' @export
#' @seealso \code{\link{rmf_read_evt}}, \code{\link{rmf_write_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}

rmf_create_evt <- function(...,
                           dis,
                           nevtop = 3,
                           ievtcb = 0,
                           surf,
                           exdp,
                           ievt = NULL,
                           ievtpf = -1
) {
  
  arg <- rmfi_create_bc_array(arg = list(...), dis = dis)
  
  # create evt object
  obj <- list()
  
  obj$dimensions <- arg$dimensions
  obj$nrchop <- nevthop
  obj$irchcb <- ievtcb
  obj$et <- arg$data
  if(arg$dimensions['np'] > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  if(nevtop == 2) {
    if(is.null(ievt)) stop('Please supply a ievt argument when nrchop = 2')
    if(!inherits(ievt, 'list')) ievt <- list(ievt)
    obj$ievt <- ievt
    names(obj$ievt) <- paste('ievt', length(ievt), sep = '_')
    obj$irch_kper <- data.frame(kper = 1:dis$nper)
    for(i in 1:length(ievt)) {
      obj$irch_kper[[paste('ievt', i, sep = '_')]] <- c(1:dis$nper) %in% attr(ievt[[i]],'kper')
    }
    # check if multiple ievt arrays are active
    ievt_err <- vapply(1:dis$nper, function(i) sum(is.na(irch_kper[i,-1]) | irch_kper[i,-1] == TRUE) > 1, TRUE)
    if(any(ievt_err)) stop(paste('There can be only 1 active ievt array per stress period. Stress period(s)', which(irch_err), 'have multiple active arrays.'))
  } 
  obj$ievtpf <- ievtpf
  
  class(obj) <- c('evt', 'rmf_package')
  return(obj)
  
}

