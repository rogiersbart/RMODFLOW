#' Read a MODFLOW time-variant specified-head file
#' 
#' \code{rmf_read_chd} reads in a MODFLOW time-variant specified-head file and returns it as an \code{RMODFLOW} chd object.
#'
#' @param file filename; typically '*.chd'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} chd object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_chd}}, \code{\link{rmf_create_chd}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?chd.htm}

rmf_read_chd <-  function(file = {cat('Please select time-variant specified-head file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('shead', 'ehead')
  option <- c('NOPRINT' = FALSE)
  lines <-  read_lines(file)
  
  input <- rmfi_read_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = c(4,5), ...)
  
  obj <- rmf_create_chd(input$rmf_lists, dis = dis, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
  
}
