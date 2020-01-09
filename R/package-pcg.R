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
#' @seealso \code{\link{rmf_read_pcg}}, \code{\link{rmf_write_pcg}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?pcg.htm}
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
                           damppcgt = 1) {
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
    if(pcg$damppcg < 0) pcg$damppcgt <- damppcgt
  
  class(pcg) <- c('pcg','rmf_package')
  return(pcg)
}

#' @describeIn rmf_create_pcg Deprecated function name
#' @export
create_pcg <- function(...) {
  .Deprecated(new = "rmf_create_pcg", old = "create_pcg")
  rmf_create_pcg(...)
}

#' Read a MODFLOW preconditioned conjugate-gradient package file
#' 
#' \code{read_pcg} reads in a MODFLOW preconditioned conjugate-gradient package file and returns it as an \code{\link{RMODFLOW}} pcg object.
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @param file filename; typically '*.pcg'
#' @return object of class pcg
#' @export
#' @seealso \code{\link{rmf_write_pcg}}, \code{\link{rmf_create_pcg}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?pcg.htm}
rmf_read_pcg <- function(file = {cat('Please select pcg file ...\n'); file.choose()}, ...) {
  
  pcg_lines <- readr::read_lines(file)
  pcg <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(pcg_lines)
  comment(pcg) <- data_set_0$comments
  pcg_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(pcg_lines)
  pcg$mxiter <- rmfi_ifelse0(is.na(data_set_1$variables[1]), 0, as.numeric(data_set_1$variables[1]))
  pcg$iter1 <- rmfi_ifelse0(is.na(data_set_1$variables[2]), 0, as.numeric(data_set_1$variables[2]))
  pcg$npcond <- rmfi_ifelse0(is.na(data_set_1$variables[3]), 0, as.numeric(data_set_1$variables[3]))
  ihcofadd <- suppressWarnings(rmfi_ifelse0(is.na(data_set_1$variables[4]), 0, as.numeric(data_set_1$variables[4])))
  pcg$ihcofadd <- ifelse(is.na(ihcofadd), 0, ihcofadd)
  pcg_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 <- rmfi_parse_variables(pcg_lines)
  pcg$hclose <- rmfi_ifelse0(is.na(data_set_2$variables[1]), 0, as.numeric(data_set_2$variables[1]))
  pcg$rclose <- rmfi_ifelse0(is.na(data_set_2$variables[2]), 0, as.numeric(data_set_2$variables[2]))
  pcg$relax <- rmfi_ifelse0(is.na(data_set_2$variables[3]), 0, as.numeric(data_set_2$variables[3]))
  pcg$nbpol <- rmfi_ifelse0(is.na(data_set_2$variables[4]), 0, as.numeric(data_set_2$variables[4]))
  pcg$iprpcg <- rmfi_ifelse0(is.na(data_set_2$variables[5]), 0, as.numeric(data_set_2$variables[5]))
  pcg$mutpcg <- rmfi_ifelse0(is.na(data_set_2$variables[6]), 0, as.numeric(data_set_2$variables[6]))
  # pcg$damp <- as.numeric(data_set_2$variables[7]) # modflow 2000 only
  pcg$damppcg <- rmfi_ifelse0(is.na(data_set_2$variables[7]), 0, as.numeric(data_set_2$variables[7]))
  if(pcg$damppcg < 0) {
    damppcgt <- suppressWarnings(rmfi_ifelse0(is.na(data_set_2$variables[8]), 0, as.numeric(data_set_2$variables[8])))
    pcg$damppcgt <- ifelse(is.na(damppcgt), 0, damppcgt)
  }
  rm(data_set_2)
  
  class(pcg) <- c('pcg','rmf_package')
  return(pcg)
}

#' @describeIn rmf_read_pcg Deprecated function name
#' @export
read_pcg <- function(...) {
  .Deprecated(new = "rmf_read_pcg", old = "read_pcg")
  rmf_read_pcg(...)
}

#' Write a MODFLOW preconditioned conjugate-gradient package file
#' 
#' @param pcg an \code{\link{RMODFLOW}} pcg object
#' @param file filename to write to; typically '*.pcg'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_pcg}}, \code{\link{rmf_create_pcg}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?pcg.htm}
rmf_write_pcg <- function(pcg,
                          file = {cat('Please select pcg file to overwrite or provide new filename ...\n'); file.choose()}, ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW preconditioned conjugate-gradient package File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(pcg)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(pcg$mxiter, pcg$iter1, pcg$npcond, ifelse(is.na(pcg$ihcofadd),'',pcg$ihcofadd),file=file, ...)
  
  # data set 2
  rmfi_write_variables(pcg$hclose, pcg$rclose, pcg$relax, pcg$nbpol, pcg$iprpcg, pcg$mutpcg, pcg$damppcg, ifelse(is.na(pcg$damppcgt) || is.null(pcg$damppcgt),'',pcg$damppcgt), file = file, ...)
}

#' @describeIn rmf_write_pcg Deprecated function name
#' @export
write_pcg <- function(...) {
  .Deprecated(new = "rmf_write_pcg", old = "write_pcg")
  rmf_write_pcg(...)
}
