#' Read a MODFLOW general-head boundary file
#' 
#' \code{rmf_read_ghb} reads in a MODFLOW general-head boundary file and returns it as an \code{RMODFLOW} ghb object.
#'
#' @param file filename; typically '*.ghb'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} ghb object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_ghb}}, \code{\link{rmf_create_ghb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?ghb.htm}


rmf_read_ghb <-  function(file = {cat('Please select general-head boundary file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('bhead', 'cond')
  option <- c('NOPRINT' = FALSE)
  lines <-  read_lines(file)
  
  input <- rmfi_read_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = 5, ...)
  
  obj <- rmf_create_ghb(input$rmf_lists, dis = dis, ighbcb = input$icb, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
}

