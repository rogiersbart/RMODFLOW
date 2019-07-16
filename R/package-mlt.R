#' Create an \code{RMODFLOW} mlt object
#' 
#' \code{rmf_create_mlt} creates an \code{RMODFLOW} mlt object
#' 
#' @param nml number of multiplier arrays to be defined; defaults to the length of mltnam
#' @param mltnam character vector of length \code{nml} specifying the names of the multiplier arrays; defaults to 'MULT'
#' @param functn optional logical vector of length \code{nml} indicating if the multiplier array will be constructed from other multiplier arrays previously defined; defaults to NULL
#' @param rmlt either a single 2d array or a list with \code{nml} 2d arrays specifying the mutliplier arrays; defaults to a \code{rmf_2d_array} with 1 for all cells
#' @param operators list with \code{nml} elements where each element is a character vector with the correct function which will be printed for that multiplier array. If no function is to be specifyied for an array, set to NULL; defaults to NULL
#' @param iprn numeric vector of length \code{nml} indicating the printing format and whether the multiplier array constructed in data set 4 will be printed to the listing file; defaults to NULL
#' 
#' @return an \code{RMODFLOW} mlt object
#' @export
#' @seealso \code{\link{rmf_read_mlt}}, \code{\link{rmf_write_mlt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?mult.htm}

rmf_create_mlt <- function(nml = length(mltnam),
                          mltnam = 'MULT',
                          functn = NULL, 
                          rmlt = rmf_create_array(1.0, dim=c(10, 10)),
                          operators = NULL,
                          iprn = NULL
                          ){
  
  mlt <- list()
  
  # data set 0
  # to provide comments, use ?comment on resulting mlt object
  
  # data set 1
  mlt$nml <-  nml
  
  # data set 2
  mlt$mltnam <-  mltnam
  if(!is.null(functn) && (T %in% functn)) mlt$functn <-  functn

  # data set 3
  if(is.null(mlt$functn) || (!is.null(mlt$functn) && (F %in% mlt$functn))) {
    if(!inherits(rmlt, 'list') && is.array(rmlt)) rmlt <- list(rmlt) 
    mlt$rmlt <- rmlt
    names(mlt$rmlt) <- mlt$mltnam[rmfi_ifelse0(is.null(functn), 1:nml, functn)]
  }
  
  # data set 4
  if(!is.null(mlt$functn) && (T %in% mlt$functn)) {
    mlt$operators <-  operators
    names(mlt$operators) <- mlt$mltnam[functn]
    mlt$iprn <-  iprn
  }
  
  class(mlt) <-  c('mlt', 'modflow_package')
  return(mlt)
  
}

#' Read a MODFLOW multiplier file
#' 
#' \code{read_mlt} reads in a MODFLOW multiplier file and returns it as an \code{\link{RMODFLOW}} mlt object.
#' 
#' @param file filename; typically '*.mlt'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return \code{RMODFLOW} mlt object
#' @export
#' @seealso \code{\link{rmf_write_mlt}}, \code{\link{rmf_create_mlt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?mult.htm}

rmf_read_mlt <- function(file = {cat('Please select mlt file ...\n'); file.choose()},
                         dis = {cat('Please select dis file ...\n'); rmf_read_dis(file.choose())},
                         ...) {
  mlt <- list()
  mlt_lines <- readr::read_lines(file)
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(mlt_lines)
  comment(mlt) <- data_set_0$comments
  mlt_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(mlt_lines)
  mlt$nml <- data_set_1$variables[1]
  mlt_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2 + 3 + 4
  mlt$rmlt <- list()
  for(i in 1:mlt$nml) {
    # data set 2
    data_set_2 <- rmfi_parse_variables(mlt_lines)
    mlt$mltnam[i] <- data_set_2$variables[1]
    mlt$functn[i] <- length(data_set_2$variables) > 1
    mlt_lines <- data_set_2$remaining_lines
    rm(data_set_2)
    
    if(is.null(mlt$functn) || (!is.null(mlt$functn) && !mlt$functn)){
      # data set 3
      data_set_3 <- rmfi_parse_array(mlt_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      mlt$rmlt[[i]] <- data_set_3$array
      names(mlt$rmlt)[i] <- mlt$mltnam[i]
      mlt_lines <- data_set_3$remaining_lines
      rm(data_set_3)
    }
    
    if(!is.null(mlt$functn) && mlt$functn[i]){
      # data set 4
      data_set_4 <- rmfi_parse_variables(mlt_lines)
      mlt$operators[i] <- paste(rmfi_parse_variables[1:(length(data_set_4$variables)-1)], sep=' ')
      names(mlt$operators)[i] <- mlt$mltnam[i]
      mlt$iprn[i] <- data_set_4$variables[length(data_set_4$variables)]
      mlt_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
    
  }
  if(!any(mlt$functn)) mlt$functn <- NULL
  
  class(mlt) <- c('mlt','rmf_package')
  return(mlt)
}

#' @describeIn rmf_read_mlt Deprecated function name
#' @export
read_mlt <- function(...) {
  .Deprecated(new = "rmf_read_mlt", old = "read_mlt")
  rmf_read_mlt(...)
}

#' Write a MODFLOW multiplier file
#'
#' \code{rmf_write_mlt} writes an MODFLOW multiplier file based on a \code{RMODFLOW} mlt object
#'
#' @param mlt an \code{RMODFLOW} mlt object
#' @param file filename to write to; typically '*.mlt'
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_mlt}}, \code{\link{rmf_create_mlt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?mult.htm}

rmf_write_mlt <-  function(mlt, file={cat('Please choose mlt file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Multiplier File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(mlt)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(mlt$nml, file=file)
  
  for (i in 1:mlt$nml){
    
    # data set 2
    rmfi_write_variables(mlt$mltnam[i], ifelse((!is.null(mlt$functn) && mlt$functn[i]), 'FUNCTION', ''), file=file)
    
    # data set 3
    if(is.null(mlt$functn) || (!is.null(mlt$functn) && !mlt$functn[i])) rmfi_write_array(mlt$rmlt[[i]], file=file, ...)
    
    # data set 4
    if(!is.null(mlt$functn) && mlt$functn[i]) rmfi_write_variables(mlt$operators[[i]], mlt$iprn[i], file=file) 
    
  }
}
