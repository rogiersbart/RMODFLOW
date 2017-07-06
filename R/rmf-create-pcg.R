#' Create an \code{RMODFLOW} pcg object
#' 
#' \code{rmf_create_pcg} creates an \code{RMODFLOW} pcg object.
#' 
#' @param mxiter maximum number of outer iterations; defaults to 20
#' @param iter1 number of inner iterations; defaults to 30
#' @param npcond flag to select matrix conditioning method (1: Modified Incomplete Cholesky (default); 2: Polynomial)
#' @param ihcofadd flag that determines what happens to an active cell that is surrounded by dry cells; defaults to 0
#' @param hclose head change criterion for convergence (length); defaults to 0.001
#' @param rclose residual criterion for convergence (cubic length per time); defaults to 0.001
#' @param relax relaxation parameter used with npcond == 1; defaults to 1
#' @param nbpol used when npcond == 2; should equal 2 for maximum eigenvalue of 2; else the estimate is calculated; defaults to 1
#' @param iprpcg printout interval for pcg; defaults to 1
#' @param mutpcg flag controlling printing information from the solver; defaults to 0
#' @param damppcg damping factor
#' @param damppcgt damping factor for transient stress periods; optional; only read when damppcg is negative
#' @return Object of class pcg
#' @export
#' @seealso \code{\link{read_pcg}}, \code{\link{write_pcg}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?pcg.htm}
rmf_create_pcg <- function(mxiter = 20,
                           iter1 = 30,
                           npcond = 1,
                           ihcofadd = 0,
                           hclose = 0.001,
                           rclose = 0.001,
                           relax = 1,
                           nbpol = 1,
                           iprpcg = 1,
                           mutpcg = 0,
                           damppcg = 1,
                           damppcgt = ifelse(damppcg < 0, 1, NA)) {
  pcg <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting pcg object
  
  # data set 1
    pcg$mxiter <- mxiter
    pcg$iter1 <- iter1
    pcg$npcond <- npcond
    pcg$ihcofadd <- ihcofadd
  
  # data set 2
    pcg$hclose <- hclose
    pcg$rclose <- rclose
    pcg$relax <- relax
    pcg$nbpol <- nbpol
    pcg$iprpcg <- iprpcg
    pcg$mutpcg <- mutpcg
    pcg$damppcg <- damppcg
    pcg$damppcgt <- damppcgt
  
  class(pcg) <- c('pcg','rmf_package')
  return(pcg)
}

#' @describeIn rmf_create_pcg Deprecated function name
create_pcg <- function(...) {
  .Deprecated(new = "rmf_create_pcg", old = "create_pcg")
  rmf_create_pcg(...)
}
