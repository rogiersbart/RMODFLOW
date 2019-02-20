#' Read a MODFLOW well file
#' 
#' \code{rmf_read_wel} reads in a MODFLOW well file and returns it as an \code{RMODFLOW} wel object.
#'
#' @param file filename; typically '*.wel'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} wel object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_wel}}, \code{\link{rmf_create_wel}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?wel.htm}

rmf_read_wel <-  function(file = {cat('Please select well file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('q')
  option <- c('NOPRINT' = FALSE)
  lines <-  read_lines(file)
  
  input <- rmfi_read_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = 4, ...)
  
  obj <- rmf_create_wel(input$rmf_lists, dis = dis, iwelcb = input$icb, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
}
