#' Read a MODFLOW river file
#' 
#' \code{rmf_read_riv} reads in a MODFLOW river file and returns it as an \code{RMODFLOW} riv object.
#'
#' @param file filename; typically '*.riv'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} riv object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_riv}}, \code{\link{rmf_create_riv}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?riv.htm}

rmf_read_riv <-  function(file = {cat('Please select river file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('stage', 'cond', 'rbot')
  option <- c('NOPRINT' = FALSE)
  lines <-  read_lines(file)
  
  input <- rmfi_read_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = 5, ...)
  
  obj <- rmf_create_riv(input$rmf_lists, dis = dis, irivcb = input$icb, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
}


