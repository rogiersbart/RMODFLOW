#' Write a MODFLOW preconditioned conjugate-gradient package file
#' 
#' @param pcg an \code{\link{RMODFLOW}} pcg object
#' @param file filename to write to; typically '*.pcg'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{read_pcg}}, \code{\link{create_pcg}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?pcg.htm}
rmf_write_pcg <- function(pcg,
                      file = {cat('Please select pcg file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW preconditioned conjugate-gradient package File created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(pcg)), sep='\n', file=file, append=TRUE)
  
  # data set 1
    rmfi_write_variables(pcg$mxiter, pcg$iter1, pcg$npcond, ifelse(is.na(pcg$ihcofadd),'',pcg$ihcofadd),file=file)
 
  # data set 2
    rmfi_write_variables(pcg$hclose, pcg$rclose, pcg$relax, pcg$nbpol, pcg$iprpcg, pcg$mutpcg, pcg$damppcg, ifelse(is.na(pcg$damppcgt),'',pcg$damppcgt), file = file)
}

#' @describeIn rmf_write_pcg Deprecated function name
#' @export
write_pcg <- function(...) {
  .Deprecated(new = "rmf_write_pcg", old = "write_pcg")
  rmf_write_pcg(...)
}
