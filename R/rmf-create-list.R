#' Add rmf list class to data.frame and check if k, i and j columns are present
#' 
#' @param df data.frame that holds at least the k, i and j columns that specify cell indices
#' @details a rmf_list is a dataframe with at least 3 integer columns k, i and j, that may contain repeated values.
#'      A 'l' column may be included which denotes time. For non-parameter lists, the l column is either the stress period number (for input files) or the time step number (for output files).
#'      For parameter lists, the l columnn is the instance name of the time-varying parameter.
#' @return an object of class \code{rmf_list} and \code{data.frame}
#' @export

rmf_create_list = function(df) {
  
  if(any(!(c('k','i','j') %in% names(df)))) stop('Please set names of the kij columns to k, i and j')
  
  class(df) = c('rmf_list', 'data.frame')
  return(df)
  
}





