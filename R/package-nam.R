#' Create an \code{RMODFLOW} nam object
#' 
#' \code{rmf_create_nam} creates an \code{RMODFLOW} nam object.
#' 
#' @param ... RMODFLOW objects to be included in the nam file
#' @param basename character specifying the basename of the files. The default (\code{NULL}) sets input basenames to 'input' and output to 'output'.
#' @return Object of class nam
#' @details if a \code{RMODFLOW nam} object is present, it is recreated.
#' @export
#' @seealso \code{\link{rmf_read_nam}}, \code{\link{rmf_write_nam}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?name_file.htm}
rmf_create_nam <- function(..., basename = NULL) {
  
  fobjects <- list(...)
  if(length(fobjects) == 1 && inherits(fobjects[[1]], c('list', 'modflow')) && !('rmf_package' %in% class(fobjects[[1]]))) fobjects <- unclass(fobjects[[1]])
  nam_path <- ''
  
  # data set 1
  df <- rmfi_list_packages(type = 'all')
  # check if all input are rmf_packages & add all input objects
  all_rmf <- vapply(fobjects, function(i) 'rmf_package' %in% class(i), TRUE)
  if(prod(all_rmf) == 0) stop('Please make sure all objects are RMODFLOW rmf_package objects representing MODFLOW input', call. = FALSE)
  classes <- vapply(fobjects, function(i) class(i)[which(class(i) == 'rmf_package')-1], 'text')
  
  # remove possible NAM object
  if('nam' %in% classes) {
    nam_path <- attr(fobjects[[which(classes == 'nam')]], 'dir')
    warning('Removing old nam object', call. = FALSE)
    fobjects <- fobjects[-which(classes == 'nam')]
    classes <- classes[-which(classes == 'nam')]
  }
  
  if(is.null(basename)) {
    input <- 'input'
    output <- 'output'
  } else {
    input <- output <- basename
  }
  
  nam <- data.frame(ftype = c('LIST', rep(NA, length(fobjects))),
                    nunit = c(700, 700 + seq_along(fobjects)),
                    fname = c(paste(output, 'lst', sep = '.'), rep(NA, length(fobjects))),
                    options = c('REPLACE', rep(NA, length(fobjects))), stringsAsFactors = FALSE)
  
  for(i in seq_along(fobjects)) {
    nam$fname[i+1] <- paste(input, classes[i], sep = '.')
    nam$ftype[i+1] <- df$ftype[classes[i] == df$rmf]
  }
  
  # check for additional output files
  # check if hed, cbc, hpr output files are required or not
  if('HOB' %in% nam$ftype) {
    hob <- fobjects[[which(nam$ftype=='HOB')-1]]
    if(hob$iuhobsv != 0) {
      nam <- rbind(nam, data.frame(ftype = 'DATA', nunit = hob$iuhobsv, fname = paste(output, 'hpr', sep = '.'), options = 'REPLACE'))  
    }
  }
  if('OC' %in% nam$ftype) {
    oc <-  fobjects[[which(nam$ftype=='OC')-1]]
    if(!is.null(oc$ihedun) && !is.na(oc$ihedun)) {
      type <- rmfi_ifelse0(is.null(oc$chedfm) || is.na(oc$chedfm), 'DATA(BINARY)', "DATA")
      nam <- rbind(nam, data.frame(ftype = type, nunit = oc$ihedun, fname = paste(output, 'hed', sep = '.'), options = 'REPLACE'))  
    } 
    if(!is.null(oc$iddnun) && !is.na(oc$iddnun)) {
      type <- rmfi_ifelse0(is.null(oc$cddnfm) || is.na(oc$cddnfm), 'DATA(BINARY)', "DATA")
      nam <- rbind(nam, data.frame(ftype = type, nunit = oc$iddnun, fname = paste(output, 'ddn', sep = '.'), options = 'REPLACE'))  
    } 
    if(!is.null(oc$ibouun) && !is.na(oc$ibouun)) {
      nam <- rbind(nam, data.frame(ftype = "DATA", nunit = oc$ibouun, fname = paste(output, 'ibound', sep = '.'), options = 'REPLACE'))  
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
      nam <- rbind(nam, data.frame(ftype = "DATA(BINARY)", nunit = cbcnum, fname = paste(output, 'cbc', sep = '.'), options = 'REPLACE')  )
    } else if(length(cbcnum) > 1){
      for(i in 1:length(cbcnum)) {
        nam <- rbind(nam, data.frame(ftype = "DATA(BINARY)", nunit = cbcnum[i], fname = paste(paste(output, i, sep = '_'), 'cbc', sep = '.'), options = 'REPLACE'))  
      }
    }
  }
  # check if solver output files are necessary
  gmg_type <- df[which(df$rmf == 'gmg'),]
  if(gmg_type$ftype %in% nam$ftype) {
    gmg <- fobjects[[which(nam$ftype == gmg_type$ftype) - 1]]
    if(gmg$iunitmhc > 0) {
      nam <- rbind(nam, data.frame(ftype = 'DATA', nunit = gmg$iunitmhc, fname = paste(output, 'mhc', sep = '.'), options = 'REPLACE'))  
    }
  }
  
  # check if ftl unit is already-used
  lmt_type <- df[which(df$rmf == 'lmt'),]
  if(lmt_type$ftype %in% nam$ftype) {
    lmt <- fobjects[[which(nam$ftype == lmt_type$ftype) - 1]]
    if(lmt$inftl %in% nam$nunit) {
      stop('lmt intfl number is already occupied by the ', df$rmf[which(df$ftype %in% nam$ftype[which(nam$nunit == lmt$inftl)[1]])], ' package. Please change the inftl value in the lmt object.', call. = FALSE)
    }
  }
  
  attr(nam, 'dir') <- nam_path
  class(nam) <- c('nam','rmf_package','data.frame')
  return(nam)
}

#' Read a MODFLOW name file
#' 
#' \code{rmf_read_nam} reads in a MODFLOW name file and returns it as an \code{\link{RMODFLOW}} nam object.
#' 
#' @param file filename; typically '*.nam'
#' @return object of class nam
#' @export
#' @seealso \code{\link{rmf_read_nam}}, \code{\link{rmf_write_nam}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?name_file.htm}
rmf_read_nam <- function(file = {cat('Please select nam file ...\n'); file.choose()}) {
  
  nam <- list()
  lines <- readr::read_lines(file)
  
  # top comments
  data_set_0 <- rmfi_parse_comments(lines)
  comments <- data_set_0$comments
  lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  indices <- rep(TRUE,length(lines))
  for(i in 1:length(lines)) {
    if(length(rmfi_remove_empty_strings(strsplit(lines[i],' |\t')[[1]])) == 0 || strsplit(rmfi_remove_empty_strings(strsplit(lines[i],' |\t')[[1]])[1], "")[[1]][1] == "#") {
      comments <-  c(comments, gsub('#', '', lines[i]))
      indices[i] <- FALSE
    } else {
      lines[i] <- rmfi_remove_comments_end_of_line(lines[i])
    }
  }
  
  nam_lines <- lines[indices]
  nam_lines <- lapply(strsplit(nam_lines, ' |\t'), rmfi_remove_empty_strings)
  nam_lines <- lapply(nam_lines, function(i) rmfi_ifelse0(length(unlist(i))< 4, c(unlist(i),NA), unlist(i)))
  
  nam <- data.frame(do.call(rbind, nam_lines), stringsAsFactors = FALSE)
  colnames(nam) <- c('ftype','nunit','fname', 'options')
  nam$nunit<- as.numeric(nam$nunit)
  nam$fname <- gsub('\"|\'', '', nam$fname)
  nam$ftype <- toupper(nam$ftype)
  
  spaces_in_fname <- !toupper(nam[[4]]) %in% c('OLD', 'REPLACE', 'UNKNOWN', NA)
  if(any(spaces_in_fname)) warning('nam$option should either be OLD, REPLACE or UNKNOWN. This warning might be generated due to whitespaces in fname which are not allowed ',
                                   '(records ', paste(which(spaces_in_fname), collapse = ', '), ')', call. = FALSE)

  if("GLOBAL" %in% nam$ftype) warning('nam file is from a MODFLOW-2000 model. RMODFLOW support for MODFLOW-2000 is limited.', call. = FALSE)
  if(any(c(5, 6, 96:99) %in% nam$nunit)) warning('nunit 5, 6, 96, 97, 98 or 99 detected. These unit numbers are not allowed by MODFLOW.', call. = FALSE)

  comment(nam) <- comments
  attr(nam, 'dir') <- dirname(file)
  class(nam) <- c('nam', 'rmf_package', 'data.frame')
  return(nam)
}

#' Write a MODFLOW name file
#' 
#' \code{rmf_write_nam} writes a MODFLOW name file based on an \code{\link{RMODFLOW}} nam object.
#' 
#' @param nam an \code{\link{RMODFLOW}} nam object
#' @param file filename to write to; typically '*.nam'
#' @param exclude character vector with packages names to exclude from the simulation. Defaults to NULL
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_nam}}, \code{\link{rmf_write_nam}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?name_file.htm}
rmf_write_nam <- function(nam,
                          file = {cat('Please select nam file to overwrite or provide new filename ...\n'); file.choose()},
                          exclude = NULL) {
  
  if(!is.null(exclude)) {
    df <- rmfi_list_packages(type = 'all')
    ftype <- df$ftype[which(df$rmf %in% exclude)]
    nam <- nam[-which(nam$ftype %in% ftype), ]
  }
  
  if(length(unique(nam$nunit)) < nrow(nam)) stop('Please make sure every file has a unique nunit specified', call. = FALSE)
  if(any(c(5, 6, 96:99) %in% nam$nunit)) stop('nunit 5, 6, 96, 97, 98 or 99 detected. These unit numbers are not allowed by MODFLOW', call. = FALSE)
  nam$nunit <- as.integer(nam$nunit)
  
  # check for spaces in fname
  if(any(grepl(' |\t', nam$fname))) stop('Whitespaces are not allowed in fname', call. = FALSE)
  spaces_in_fname <- !toupper(nam$option) %in% c('OLD', 'REPLACE', 'UNKNOWN', NA)
  if(any(spaces_in_fname)) stop('nam$option should either be OLD, REPLACE or UNKNOWN. This error might be generated due to whitespaces in fname which are not allowed ',
                                   '(records ', paste(which(spaces_in_fname), collapse = ', '), ')', call. = FALSE)
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Name File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(nam)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  # write.table(nam, file = file, row.names = FALSE, col.names = FALSE, quote = FALSE, na='', append=TRUE)
  readr::write_tsv(nam, path = file, append = TRUE, col_names = FALSE, quote_escape = FALSE, na = '')
}
