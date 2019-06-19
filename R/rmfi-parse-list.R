
#' Reads a MODFLOW list
#'@param nlst number of list rows to read
#'@param l stress period number
#'@param varnames character vector; names of the variables starting from the 4th column (so after ijk). Length of varnames is used to dimension the dataframe
#'@param scalevar column name or integer; this column will be scaled
#'@param file the file that is being read; needed if list is specified through an OPEN/CLOSE statement
#'@param format either 'fixed' or 'free'
#'@param ... ignored
#'@keywords internal

rmfi_parse_list <-  function(remaining_lines, nlst, l = NULL, varnames, scalevar=4, file,  format = 'free', precision = 'single', ...) {
  
  header <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(remaining_lines[1]),' |\t')[[1]])
  n <- 3 + length(varnames) 
  real_number_bytes <- ifelse(precision == 'single', 4, 8)
  scale <-  1.0
  df <-  matrix(nrow=nlst, ncol=3+length(varnames))
  
  if(toupper(header[1]) == 'EXTERNAL') {
    if(is.null(nam)) stop('List is read on an EXTERNAL file. Please supply the nam object')
    remaining_lines <-  remaining_lines[-1]
    extfile <- paste(attr(nam, 'dir'), nam$fname[which(nam$nunit==as.numeric(header[2]))], sep='/')
    binary <-  ifelse(toupper(nam$ftype[which(nam$nunit==as.numeric(header[2]))]) == 'DATA(BINARY)', TRUE, FALSE)
    
    if(binary) {
      con <- file(extfile, open='rb')
      aa = readBin(con, what = 'numeric', n =nlst*(3+length(varnames)), size = real_number_bytes)
      close(con)
      df = matrix(aa, nrow=nlst, ncol=3+length(varnames), byrow = TRUE)
    } else {
      ext_lines <- readr::read_lines(extfile)
      header <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(ext_lines[1]),' |\t')[[1]])
      if(toupper(header[1]) == 'SFAC') {
        scale <- as.numeric(header[2])
        ext_lines <- ext_lines[-1]
      }
      for(nl in 1:nlst) {
        values <-  rmfi_parse_variables(remaining_lines = ext_lines, n = n, format = format)$variables
        if(format=='fixed') values[which(is.na(values[1:(3+length(varnames))]))] <- 0
        df[nl,] <- values[1:(3+length(varnames))]
        ext_lines <- ext_lines[-1]
      }
    }
    
  } else if(toupper(header[1]) == 'OPEN/CLOSE') {
    remaining_lines <-  remaining_lines[-1]
    extfile <- paste(file, as.character(header[2]), sep='/')
    binary <- rmfi_ifelse0(!is.na(header[3]) && toupper(header[3]) == "(BINARY)", TRUE, FALSE)
    
    if(binary) {
      con <- file(extfile, open='rb')
      aa = readBin(con, what = 'numeric', n =nlst*(3+length(varnames)), size = real_number_bytes)
      close(con)
      df = matrix(aa, nrow=nlst, ncol=3+length(varnames), byrow = TRUE)
    } else {
      
      ext_lines <- readr::read_lines(extfile)
      
      header <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(ext_lines[1]),' |\t')[[1]])
      if(toupper(header[1]) == 'SFAC') {
        scale <- as.numeric(header[2])
        ext_lines <- ext_lines[-1]
      }
      for(nl in 1:nlst) {
        values <-  rmfi_parse_variables(remaining_lines = ext_lines, n = n, format = format)$variables
        if(format=='fixed') values[which(is.na(values[1:(3+length(varnames))]))] <- 0
        df[nl,] <- values[1:(3+length(varnames))]
        ext_lines <- ext_lines[-1]
      }
    }
    
  } else if(toupper(header[1]) == 'SFAC') {
    remaining_lines <- remaining_lines[-1]
    scale <- as.numeric(header[2])
    for(nl in 1:nlst) {
      values <-  rmfi_parse_variables(remaining_lines = remaining_lines, n = n, format = format)$variables
      if(format=='fixed') values[which(is.na(values[1:(3+length(varnames))]))] <- 0
      df[nl,] <- values[1:(3+length(varnames))]
      remaining_lines <- remaining_lines[-1]
    }
  } else {
    for(nl in 1:nlst) {
      values <-  rmfi_parse_variables(remaining_lines = remaining_lines, n = n, format = format)$variables
      if(format=='fixed') values[which(is.na(values[1:(3+length(varnames))]))] <- 0
      df[nl,] <- values[1:(3+length(varnames))]
      remaining_lines <- remaining_lines[-1]
    }
  }
  
  df <- data.frame(df)
  colnames(df) <- c('k','i','j',varnames)
  if(!is.null(l)) df$l <- l
  class(df) <- c('rmf_list', 'data.frame')
  if(scale != 1.0) df[scalevar] <- scale*df[scalevar]
  
  return(list(list = df, remaining_lines = remaining_lines))
  
}
