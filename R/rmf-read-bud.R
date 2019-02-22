#' Reads the volumetric budget from a MODFLOW listing file
#'
#' \code{rmf_read_bud} reads a volumetric budget from a MODFLOW listing file and returns it as a list with data frame elements
#' 
#' @param file path to the listing file; typically '*.lst'
#'
#' @return an object of class bud which is a list with two data frames: one with cumulative fluxes and one with rates
#' @export
#' @importFrom readr read_lines

rmf_read_bud <-  function(file = {cat('Please select listing file ...\n'); file.choose()}){
  
  lst.lines <- read_lines(file)
  headers <- grep("VOLUMETRIC BUDGET FOR ENTIRE MODEL", lst.lines)
  enders <- grep("TIME SUMMARY AT END OF TIME STEP", lst.lines)
  
  # if budget is printed
  if(length(headers) > 0) {
    
    # helper functions
    read_vars <- function(index, lines) rmfi_remove_empty_strings(strsplit(lines[index], ' ')[[1]])
    get_timing <- function(header_vector) {
      kstp <- as.numeric(strsplit(header_vector[11],',')[[1]])
      kper <- as.numeric(header_vector[14])
      # nstp <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper - 1] + kstp)
      return(list(kstp, kper))
    }
    get_vars <- function(var_vector) {
      breaks <- which(var_vector == '=')
      #name <- paste(tolower(var_vector[1:(breaks[1] - 1)]), collapse = "_")
      cuml <-  as.numeric(var_vector[breaks[1] + 1])
      rate <-  as.numeric(var_vector[breaks[2] + 1])
      return(c(cuml,rate))
    }
    get_balance <- function(lines) {
      vars <- lapply(c((1:nr) + strt.in - 1, tot.in, (1:nr) + strt.out - 1, tot.out, inout, discrp),
                     function(i) read_vars(index = i, lines = lines))
      
      m <- do.call(cbind, c(get_timing(read_vars(index = 1, lines = lines)), 
                            lapply(vars, get_vars)))
      return(list(cuml = m[1,], rate = m[2,]))
    } 
    break_names <- function(var_vector) {
      breaks <- which(var_vector == '=')
      name <- paste(tolower(var_vector[1:(breaks[1] - 1)]), collapse = "_")
    }
    get_names <- function(lines) {
      vars <- lapply(c((1:nr) +strt.in - 1), function(i) read_vars(index = i, lines = lines))
      names <- unlist(lapply(vars, break_names))
      c('kstp', 'kper', paste(names, 'in', sep='_'), 'total_in',
        paste(names, 'out', sep='_'), 'total_out', 'difference', 'discrepancy')
    }
    
    # call  
    # set indices based on first budget
    lines <- lst.lines[headers[1]:enders[1]]
    strt.in <- 9
    tot.in <- grep('TOTAL IN', lines)
    end.in <- tot.in - 2
    strt.out <- tot.in + 4
    tot.out <- grep('TOTAL OUT', lines)
    end.out <- tot.out - 2
    inout <- tot.out + 2
    discrp <- inout + 2
    # number of variables; same in IN as in OUT
    nr <- (end.in - strt.in) + 1
    names <- get_names(lines)
    
    balance <- lapply(seq_along(headers), function(i) get_balance(lst.lines[headers[i]:enders[i]]))
    balance <-  list(cumulative = as.data.frame(do.call(rbind,lapply(balance, '[[', 'cuml'))), 
                   rates =     as.data.frame(do.call(rbind,lapply(balance, '[[', 'rate'))))
    colnames(balance$cumulative) <- colnames(balance$rates) <- names
    
    
    # no budget is printed
  } else {
    stop("No budget was printed to listing file. You can change this in the OC file.")
  }
  
  class(balance) = 'bud'
  return(balance)
  
}


#' @describeIn rmf_read_bud
#' @export
rmf_read_budget <- function(...) {
  rmf_read_bud(...)
}
