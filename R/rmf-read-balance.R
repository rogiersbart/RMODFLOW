#' Reads the volumetric budget from a MODFLOW listing file
#'
#' \code{rmf_read_balance} reads in a volumetric budget from a MODFLOW listing file and returns it as a list with data frame elements
#' 
#' @param list_path path to the listing file; typically '*.lst'; defaults to 'output.lst' in the working directory
#'
#' @return an object of class balance which is a list with the data frame elements holding the volumetric balance values
#' @export

rmf_read_balance=function(list_path=paste(getwd(), "output.lst", sep='/')){
  
  list_lines = readLines(list_path)
  balance_first_lines=which(grepl("VOLUMETRIC BUDGET FOR ENTIRE MODEL", list_lines))
  balance_last_lines=which(grepl("TIME SUMMARY AT END OF TIME STEP", list_lines))-7
  
  
  balances=vector('list', length(balance_first_lines))
  for (i in 1:length(balance_first_lines)){
    
    balances[[i]] = list_lines[balance_first_lines[i]:balance_last_lines[i]]
    
  }
  
  
  balances=lapply(balances, rmfi_get_balance)
  
  class(balances) = 'balance'
  return(balances)
  
}