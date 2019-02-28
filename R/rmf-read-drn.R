#' Read a MODFLOW drain file
#' 
#' \code{rmf_read_drn} reads in a MODFLOW drain file and returns it as an \code{RMODFLOW} drn object.
#'
#' @param file filename; typically '*.drn'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} drn object
#' @importFrom readr read_lines
#' @seealso \code{\link{rmf_write_drn}}, \code{\link{rmf_create_drn}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?drn.htm}

rmf_read_drn <-  function(file = {cat('Please select drain file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('elevation', 'cond')
  option <- c('NOPRINT' = FALSE)
  lines <-  read_lines(file)
  
  input <- rmfi_read_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = 5, ...)
  
  obj <- rmf_create_drn(input$rmf_lists, dis = dis, idrncb = input$icb, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
}
