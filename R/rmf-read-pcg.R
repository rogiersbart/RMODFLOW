#' Read a MODFLOW preconditioned conjugate-gradient package file
#' 
#' \code{read_pcg} reads in a MODFLOW preconditioned conjugate-gradient package file and returns it as an \code{\link{RMODFLOW}} pcg object.
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @param file filename; typically '*.pcg'
#' @return object of class pcg
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{write_pcg}}, \code{\link{create_pcg}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?pcg.htm}
rmf_read_pcg <- function(file = {cat('Please select pcg file ...\n'); file.choose()}, ...) {
  
  pcg_lines <- read_lines(file)
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
    pcg$ihcofadd <-rmfi_ifelse0(is.na(data_set_1$variables[4]), 0, as.numeric(data_set_1$variables[4]))
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
    pcg$damppcgt <- rmfi_ifelse0(is.na(data_set_2$variables[8]), 0, as.numeric(data_set_2$variables[8]))
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
