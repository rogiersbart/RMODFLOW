#' Create an \code{RMODFLOW} nam object
#' 
#' \code{rmf_create_nam} creates an \code{RMODFLOW} nam object.
#' 
#' @param ... RMODFLOW objects to be included in the nam file
#' @return Object of class nam
#' @export
#' @seealso \code{\link{rmf_read_nam}}, \code{\link{rmf_write_nam}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?name_file.htm}
rmf_create_nam <- function(...) {
  
  fobjects <- list(...)
  if(length(fobjects) == 1 && inherits(fobjects[[1]], 'list') && !('rmf_package' %in% class(fobjects[[1]]))) fobjects <- fobjects[[1]]
  
  nam <- data.frame(ftype = c('LIST',rep(NA, length(fobjects))),
                    nunit = c(700, 700 + seq_along(fobjects)),
                    fname = c('output.lst',rep(NA, length(fobjects))),
                    options = rep(NA, 1 + length(fobjects)), stringsAsFactors = FALSE)
  
  # data set 1
  df <- rmfi_list_packages(type = 'all')
  # add all input objects
  classes <- vapply(fobjects, function(i) class(i)[which(class(i) == 'rmf_package')-1], 'text')
  for(i in seq_along(fobjects)) {
    nam$fname[i+1] <- paste0('input.',classes[i])
    nam$ftype[i+1] <- df$ftype[classes[i] == df$rmf]
  }

  # check for additional output files
  # check if hed, cbc, hpr output files are required or not
  if('HOB' %in% nam$ftype) {
    hob <- fobjects[[which(nam$ftype=='HOB')-1]]
    if(hob$iuhobsv != 0) {
      nam <- rbind(nam, data.frame(ftype = 'DATA', nunit = hob$iuhobsv, fname = 'output.hpr', options = NA))  
    }
  }
  if('OC' %in% nam$ftype) {
    oc <-  fobjects[[which(nam$ftype=='OC')-1]]
    if(!is.null(oc$ihedun) && !is.na(oc$ihedun)) {
      type <- rmfi_ifelse0(is.null(oc$chedfm) || is.na(oc$chedfm), 'DATA(BINARY)', "DATA")
      nam <- rbind(nam, data.frame(ftype = type, nunit = oc$hedun, fname = 'output.hed', options = NA))  
    } 
    if(!is.null(oc$iddnun) && !is.na(oc$iddnun)) {
      type <- rmfi_ifelse0(is.null(oc$cddnfm) || is.na(oc$cddnfm), 'DATA(BINARY)', "DATA")
      nam <- rbind(nam, data.frame(ftype = type, nunit = oc$iddnun, fname = 'output.ddn', options = NA))  
    } 
    if(!is.null(oc$ibouun) && !is.na(oc$ibouun)) {
      nam <- rbind(nam, data.frame(ftype = "DATA", nunit = oc$ibouun, fname = 'output.ibound', options = NA))  
    }
    
    #CBC
    cbc_packages <- rmfi_list_packages(type = 'cbc')
    cbcnum <-  vector(mode = "integer")
    for(i in 1:length(cbc_packages$ftype)) {
      if(cbc_packages$ftype[i] %in% nam$ftype) {
        obj <- fobjects[[which(nam$ftype==cbc_packages$ftype[i])-1]]
        # some packages have i*cb1, i*cb2. SWI has iswibd
        cbc_base <- paste0('i',df$rmf[which(df$ftype == cbc_packages$ftype[i])],'cb')
        cbcnum <-  c(cbcnum, c(obj[[cbc_base]], obj[[paste0(cbc_base, '1')]], obj[[paste0(cbc_base, '2')]], obj[[paste0('i',df$rmf[which(df$ftype == cbc_packages$ftype[i])],'bd')]]))
      }
    }
    cbcnum <-  unique(cbcnum[cbcnum > 0])
    if(length(cbcnum) == 1) {
      nam <- rbind(nam, data.frame(ftype = "DATA(BINARY)", nunit = cbcnum, fname = 'output.bud', options = NA)  )
    } else {
      for(i in 1:length(cbcnum)) {
        nam <- rbind(nam, data.frame(ftype = "DATA(BINARY)", nunit = cbcnum[i], fname = paste0('output_',i,'.bud'), options = NA))  
      }
    }
  }
  
  # set REPLACE option for binary files
  nam$options[which(toupper(nam$ftype) == 'DATA(BINARY)')] <- 'REPLACE'
      
  attr(nam, 'dir') <-  getwd()
  class(nam) <- c('nam','rmf_package','data.frame')
  return(nam)
}

#' @describeIn rmf_create_nam Deprecated function name
#' @export
create_nam <- function(...) {
  .Deprecated(new = "rmf_create_nam", old = "create_nam")
  rmf_create_nam(...)
}

#' Read a MODFLOW name file
#' 
#' \code{rmf_read_nam} reads in a MODFLOW name file and returns it as an \code{\link{RMODFLOW}} nam object.
#' 
#' @param file filename; typically '*.nam'
#' @return object of class nam
#' @export
rmf_read_nam <- function(file = {cat('Please select nam file ...\n'); file.choose()}) {
  
  nam <- list()
  lines <- readr::read_lines(file)
  
  # top comments
  data_set_0 <- rmfi_parse_comments(lines)
  comments <- data_set_0$comments
  lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  indices <- rep(T,length(lines))
  for(i in 1:length(lines)) {
    if(length(rmfi_remove_empty_strings(strsplit(lines[i],' |\t')[[1]])) == 0 || strsplit(rmfi_remove_empty_strings(strsplit(lines[i],' |\t')[[1]])[1], "")[[1]][1] == "#") {
      comments <-  c(comments, gsub('#', '', lines[i]))
      indices[i] <-  FALSE
    } else {
      lines[i] <- rmfi_remove_comments_end_of_line(lines[i])
    }
  }
  nam_lines <- lines[indices]
  nam_lines <- lapply(strsplit(nam_lines, ' |\t'), rmfi_remove_empty_strings)
  nam_lines <- lapply(nam_lines, function(i) rmfi_ifelse0(length(unlist(i))< 4, c(unlist(i),NA), unlist(i)))
  
  nam <-  data.frame(do.call(rbind, nam_lines), stringsAsFactors = F)
  colnames(nam) <- c('ftype','nunit','fname', 'options')
  nam$nunit<- as.numeric(nam$nunit)
  nam$fname <- gsub('"', '', nam$fname, fixed = TRUE)
  
  comment(nam) <- comments
  attr(nam, 'dir') <- dirname(file)
  class(nam) <- c('nam','data.frame')
  return(nam)
}

#' @describeIn rmf_read_nam Deprecated function name
#' @export
read_nam <- function(...) {
  .Deprecated(new = "rmf_read_nam", old = "read_nam")
  rmf_read_nam(...)
}

#' Write a MODFLOW name file
#' 
#' \code{rmf_write_nam} writes a MODFLOW name file based on an \code{\link{RMODFLOW}} nam object.
#' 
#' @param nam an \code{\link{RMODFLOW}} nam object
#' @param file filename to write to; typically '*.nam'
#' @return \code{NULL}
#' @export
rmf_write_nam <- function(nam,
                          file = {cat('Please select nam file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Name File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(nam)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  write.table(nam, file = file, row.names = FALSE, col.names = FALSE, quote = FALSE, na='', append=T)
}

#' @describeIn rmf_write_nam Deprecated function name
#' @export
write_nam <- function(...) {
  .Deprecated(new = "rmf_write_nam", old = "write_nam")
  rmf_write_nam(...)
}
