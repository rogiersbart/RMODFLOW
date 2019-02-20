#' Add rmf list class to data.frame and check if k, i and j columns are present
#' 
#' @param df data.frame that holds at least the k, i and j columns that specify cell indices as well as additional variables related to the boundary condition package.
#' @param kper numeric vector with the stress period numbers during which the list is active.
#' @details 
#'      rmf_lists represent List Data input (and output) as used by MODFLOW. rmf_lists are used to define stress period input for boundary condition packages; 
#'      to define parameters and to read certain types of output in the the cell-by-cell budget file. A MODFLOW List Data can be viewed as discrete spatial features in contrast to the MODFLOW array data type which represents continuous data.
#'      
#'      A rmf_list is a dataframe with at least 3 integer columns k, i and j, that may contain repeated values.
#'      
#' @return an object of class \code{rmf_list} and \code{data.frame}
#' @export

rmf_create_list <-  function(df, kper, name = NULL) {
  
  if(any(!(c('k','i','j') %in% names(df)))) stop('Please set names of the kij columns to k, i and j')
  df <- as.data.frame(df)
  colnames(df) <- tolower(colnames(df))
  attr(df, 'kper') <- kper  
  class(df) = c('rmf_list', class(df))
  return(df)
  
}

