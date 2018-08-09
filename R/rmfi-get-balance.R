#' Converts MODLFOW budget lines from the listing file into a data frame
#' 
#' @param balance_lines character vector of length n where each element is a line of a MODFLOW listing file (txt) with n lines. The first line should be "VOLUMETRIC BUDGET OF THE ENTIRE MODEL AT TIME STEP K, STRESS PERIOD M"; the last line should be "PERCENT DISCREPANCY = ..." 
#' @return a data frame with the mass balance
#' @keywords internal  

rmfi_get_balance=function(balance_lines){ # balance lines is a character vector of length n where each element is a line of a text file with n lines. The first line should be "VOLUMETRIC BUDGET OF THE ENTIRE MODEL AT TIME STEP K, STRESS PERIOD M"; the last line should be "PERCENT DISCREPANCY = ..."
  
  header = balance_lines[1]
  header_lines = rmfi_remove_empty_strings(strsplit(header, ' ')[[1]])
  
  
  balance_lines_inflow =  balance_lines[(which(grepl('IN:', balance_lines))+2):(which(grepl('OUT:', balance_lines))-2)]
  balance_lines_outflow = balance_lines[(which(grepl('OUT:', balance_lines))+2):(which(grepl('TOTAL OUT =', balance_lines)))]
  difference_lines = balance_lines[which(grepl('IN - OUT', balance_lines))]
  perc_discrepancy_lines = balance_lines[which(grepl('PERCENT DISCREPANCY =', balance_lines))]
  
  
  balance=list()
  
  # data frame with the balance
  balance$df = as.data.frame(array(NA, dim=c(4, (length(balance_lines_inflow)-2)) ) )
  row.names(balance$df) = c('cuml_in', 'cuml_out', 'rate_in', 'rate_out')
  column_names=vector(mode="character")
  
  for (i in 1:(length(balance_lines_inflow)-2)){
    
    tmp_in = rmfi_remove_empty_strings(strsplit(balance_lines_inflow[i], ' ')[[1]])
    tmp_out = rmfi_remove_empty_strings(strsplit(balance_lines_outflow[i], ' ')[[1]])
    
    balance$df[1, i] = as.numeric(tmp_in[length(tmp_in)/2]) 
    balance$df[3, i] = as.numeric(tmp_in[length(tmp_in)])
    
    balance$df[2, i] = as.numeric(tmp_out[length(tmp_out)/2])
    balance$df[4, i] = as.numeric(tmp_out[length(tmp_out)])
    
    column_names = append(column_names, as.character(paste(tmp_in[1:((length(tmp_in)/2)-2)], collapse=' ')) )
    
    rm(tmp_in, tmp_out)
  }
  
  colnames(balance$df) = column_names
  
  # vector with the IN - OUT difference
  difference_lines_split =  rmfi_remove_empty_strings(strsplit(difference_lines, ' ')[[1]])
  balance$diff = c('cuml_diff' = as.numeric(difference_lines_split[length(difference_lines_split)/2]), 'rate_diff' =  as.numeric(difference_lines_split[length(difference_lines_split)]))
  
  # vector with the percent discrepancy
  perc_discrepancy_lines_split = rmfi_remove_empty_strings(strsplit(perc_discrepancy_lines, ' ')[[1]])
  balance$discr = c('cuml_discr' = as.numeric(perc_discrepancy_lines_split[length(perc_discrepancy_lines_split)/2]), 'rate_discr' = as.numeric(perc_discrepancy_lines_split[length(perc_discrepancy_lines_split)]) )
  
  attributes(balance)=list('sp'= as.numeric(header_lines[length(header_lines)]), 'ts'= as.numeric(paste(strsplit(header_lines[length(header_lines)-3], '')[[1]][1:(which(strsplit(header_lines[length(header_lines)-3], '')[[1]]==',')-1)], collapse="" )) )
  
  
  return(balance)
  
}