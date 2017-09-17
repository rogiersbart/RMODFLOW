#' Read a MODFLOW multiplier file
#' 
#' \code{read_mlt} reads in a MODFLOW multiplier file and returns it as an \code{\link{RMODFLOW}} mlt object.
#' 
#' @param file filename; typically '*.mlt'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' 
#' @return \code{RMODFLOW} mlt object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_mlt}}, \code{\link{rmf_create_mlt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?mult.htm}

rmf_read_mlt <- function(file = {cat('Please select mlt file ...\n'); file.choose()},
                         dis = {cat('Please select dis file ...\n'); rmf_read_dis(file.choose())}) {
  mlt <- list()
  mlt_lines <- read_lines(file)
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(mlt_lines)
  comment(mlt) <- data_set_0$comments
  mlt_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 = rmfi_parse_variables(mlt_lines)
  mlt$nml = data_set_1$variables[1]
  mlt_lines = data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2 + 3 + 4
  mlt$rmlt <- array(dim=c(dis$nrow, dis$ncol, mlt$nml))
  for(i in 1:mlt$nml) {
   # data set 2
   data_set_2 = rmfi_parse_variables(mlt_lines)
   mlt$mltnam[i] = data_set_2$variables[1]
   if(length(data_set_2$variables) > 1) mlt$functn[i]= T else mlt$functn[i] = F
   mlt_lines = data_set_2$remaining_lines
   rm(data_set_2)
   
   if(is.null(mlt$functn) || (!is.null(mlt$functn) && !mlt$functn)){
     # data set 3
     data_set_3 = rmfi_parse_array(mlt_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1)
     mlt$rmlt[,,i] = data_set_3$array
     mlt_lines = data_set_3$remaining_lines
     rm(data_set_3)
   }
   
   if(!is.null(mlt$functn) && mlt$functn[i]){
     # data set 4
     data_set_4 = rmfi_parse_variables(mlt_lines)
     mlt$operators[i] = paste(rmfi_parse_variables[1:(length(data_set_4$variables)-1)], sep=' ')
     mlt$iprn[i] = data_set_4$variables[length(data_set_4$variables)]
     mlt_lines = data_set_4$remaining_lines
     rm(data_set_4)
   }
  
  }
  if(!any(mlt$functn)) mlt$functn = NULL
  
  class(mlt) <- c('mlt','rmf_package')
  return(mlt)
}

#' @describeIn rmf_read_mlt Deprecated function name
#' @export
read_mlt <- function(...) {
  .Deprecated(new = "rmf_read_mlt", old = "read_mlt")
  rmf_read_mlt(...)
}