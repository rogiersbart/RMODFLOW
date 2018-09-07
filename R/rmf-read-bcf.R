#' Read a MODFLOW bcf file
#' 
#' \code{rmf_read_bcf} reads in a MODFLOW block-centered flow file and returns it as an \code{RMODFLOW} bcf object
#' 
#' @param file filename; typically '*_bcf'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_array} and \code{rmfi_parse_variables}. Can be ignored when input is 'free' format and arrays are INTERNAL or CONSTANT.
#' @return an \code{RMODFLOW} bcf object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_bcf}}, \code{\link{rmf_create_bcf}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}

rmf_read_bcf = function(file = {cat('Please select bcf file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                        ...){
  
  bcf <-  list()
  bcf_lines <-  readr::read_lines(file)
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(bcf_lines)
  comment(bcf) <-  data_set_0$comments
  bcf_lines <-  data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(bcf_lines, ...)
  bcf$ibcfcb <- rmfi_ifelse0(is.na(data_set_1$variables[1]), 0, as.numeric(data_set_1$variables[1]))
  bcf$hdry <- rmfi_ifelse0(is.na(data_set_1$variables[2]), 0, as.numeric(data_set_1$variables[2]))
  bcf$iwdflg <- rmfi_ifelse0(is.na(data_set_1$variables[3]), 0, as.numeric(data_set_1$variables[3]))
  bcf$wetfct <- rmfi_ifelse0(is.na(data_set_1$variables[4]), 0, as.numeric(data_set_1$variables[4]))
  bcf$iwetit <- rmfi_ifelse0(is.na(data_set_1$variables[5]), 0, as.numeric(data_set_1$variables[5]))
  bcf$ihdwet <- rmfi_ifelse0(is.na(data_set_1$variables[6]), 0, as.numeric(data_set_1$variables[6]))
  bcf_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  counter <- 0
  format <- ifelse('format' %in% names(list(...)), get('format'), 'free')
  
  while(counter != dis$nlay) {
    if(format == 'free') {
      data_set_2 <- rmfi_parse_variables(bcf_lines, ...)
      bcf_lines <- data_set_2$remaining_lines
      data_set_2 <- data_set_2$variables
    } else if(format == 'fixed') {
      data_set_2 <- (unlist(lapply(seq(1,nchar(bcf_lines[1]), by=2), 
                                  function(i) paste0(strsplit(rmfi_remove_comments_end_of_line(bcf_lines[1]),'')[[1]][i:(i+1)], collapse=''))))
      data_set_2 <- lapply(strsplit(data_set_2, " |t"), rmfi_remove_empty_strings)
      data_set_2[which(lengths(data_set_2)==0)] = 0 # empty values are set to 0
      bcf_lines <- bcf_lines[-1]
    }
    data_set_2 <- strsplit(as.character(data_set_2), split = '')
    data_set_2 <- lapply(data_set_2, function(i) rmfi_ifelse0(length(unlist(i)) == 1, c("0",unlist(i)), i))
    
    while(counter != length(data_set_2) && counter != dis$nlay) {
      counter <- counter+1
      bcf$layavg[counter] <- as.numeric(data_set_2[[counter]][1])
      bcf$laycon[counter] <- as.numeric(data_set_2[[counter]][2])
    }
    rm(data_set_2)
  }
  rm(counter)

  
  # data set 3
  data_set_3 <- rmfi_parse_array(bcf_lines, 1, dis$nlay, 1, ndim = 1, file = file, ...)
  bcf$trpy <- data_set_3$array
  bcf_lines <- data_set_3$remaining_lines
  rm(data_set_3)
  
  # data set 4-9
  bcf$wetdry <- bcf$sf2 <- bcf$vcont <- bcf$hy <- bcf$tran <- bcf$sf1 <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  
  for(i in 1:dis$nlay){
    
    # data set 4
    if('TR' %in% dis$sstr){
      data_set_4 <- rmfi_parse_array(bcf_lines, nrow=dis$nrow, ncol = dis$ncol, nlay=1, file = file, ...)
      bcf$sf1[,,i] <- data_set_4$array
      bcf_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
    
    # data set 5
    if(bcf$laycon[i] %in% c(0,2)){
      data_set_5 <- rmfi_parse_array(bcf_lines, nrow=dis$nrow, ncol=dis$ncol, nlay=1, file = file, ...)
      bcf$tran[,,i] <- data_set_5$array
      bcf_lines <- data_set_5$remaining_lines
      rm(data_set_5)
    }
    
    # data set 6
    if(bcf$laycon[i] %in% c(1,3)){
      data_set_6 <- rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      bcf$hy[,,i] <- data_set_6$array
      bcf_lines <- data_set_6$remaining_lines
      rm(data_set_6)
    }
    
    # data set 7
    if(i != dis$nlay){
      data_set_7 <- rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      bcf$vcont[,,i] <- data_set_7$array
      bcf_lines <- data_set_7$remaining_lines
      rm(data_set_7)
    }
    
    # data set 8
    if(('TR' %in% dis$sstr) && bcf$laycon[i] %in% c(2,3)){
      data_set_8 <- rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      bcf$sf2[,,i] <- data_set_8$array
      bcf_lines <- data_set_8$remaining_lines
      rm(data_set_8)
    }
    
    # data set 9
    if((bcf$iwdflg != 0) && (bcf$laycon[i] %in% c(1,3))){
      data_set_9 <- rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      bcf$wetdry[,,i] <- data_set_9$array
      bcf_lines <- data_set_9$remaining_lines
      rm(data_set_9)
    } 
  }
  
  if(all(is.na(bcf$sf1))) bcf$sf1 <- NULL
  if(all(is.na(bcf$tran))) bcf$tran <- NULL
  if(all(is.na(bcf$hy))) bcf$hy <- NULL
  if(all(is.na(bcf$sf2))) bcf$sf2 <- NULL
  if(all(is.na(bcf$wetdry))) bcf$wetdry <- NULL
  
  class(bcf) <- c('bcf', 'rmf_package')
  return(bcf)
  
}