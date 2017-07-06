#' Read a MODFLOW preconditioned conjugate-gradient package file
#' 
#' \code{read_pcg} reads in a MODFLOW preconditioned conjugate-gradient package file and returns it as an \code{\link{RMODFLOW}} pcg object.
#' 
#' @param file filename; typically '*.pcg'
#' @return object of class pcg
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{write_pcg}}, \code{\link{create_pcg}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?pcg.htm}
rmf_read_pcg <- function(file = {cat('Please select pcg file ...\n'); file.choose()}) {
  
  pcg_lines <- read_lines(file)
  pcg <- list()
  
  # data set 0
    data_set_0 <- rmfi_parse_comments(pcg_lines)
    comment(pcg) <- data_set_0$comments
    pcg_lines <- data_set_0$remaining_lines
    rm(data_set_0)
    
  # data set 1
    data_set_1 <- rmfi_parse_variables(pcg_lines)
    pcg$mxiter <- as.numeric(data_set_1$variables[1])
    pcg$iter1 <- as.numeric(data_set_1$variables[2])
    pcg$npcond <- as.numeric(data_set_1$variables[3])
    pcg$ihcofadd <- as.numeric(data_set_1$variables[4])
    pcg_lines <- data_set_1$remaining_lines
    rm(data_set_1)
    
  # data set 2
    data_set_2 <- rmfi_parse_variables(pcg_lines)
    pcg$hclose <- as.numeric(data_set_2$variables[1])
    pcg$rclose <- as.numeric(data_set_2$variables[2])
    pcg$relax <- as.numeric(data_set_2$variables[3])
    pcg$nbpol <- as.numeric(data_set_2$variables[4])
    pcg$iprpcg <- as.numeric(data_set_2$variables[5])
    pcg$mutpcg <- as.numeric(data_set_2$variables[6])
    # pcg$damp <- as.numeric(data_set_2$variables[7]) # modflow 2000 only
    pcg$damppcg <- as.numeric(data_set_2$variables[7])
    pcg$damppcgt <- as.numeric(data_set_2$variables[8])
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
