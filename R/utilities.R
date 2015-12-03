#' Split a string in numbers.
#' 
#' @param string A string.
#' @return Vector of numbers within the string, without comments at the end of the string.
split_line_numbers <- function(string)
{
  split.line <- as.vector(na.omit(as.numeric(strsplit(remove_comments_end_of_line(string),' |\t')[[1]])))
  return(split.line)
}
#' Split a string in words.
#' 
#' @param string A string.
#' @return Vector of words within the string, without comments at the end of the string.
split_line_words <- function(string)
{
  split.line <- as.character(strsplit(remove_comments_end_of_line(string),' |\t')[[1]])
  split.line <- remove_empty_strings(split.line)
  return(split.line)
}
#' Remove empty elements from a vector of strings.
#' 
#' @param stringArray Vector of strings.
#' @return Vector of strings without the empty items.
remove_empty_strings <- function(stringArray)
{
  return(stringArray[which(stringArray!='')])
}
#' Remove comments at the beginning of a vector of strings
#' 
#' @param lines Vector of strings, typically obtained from using \code{scan} with \code{sep='\n'}.
#' @return Vector of strings without the commented lines.
remove_comments_from_lines <- function(lines)
{
  i <- 0
  while(i==0) ifelse(substr(lines[1], 1,1)=='#', lines <- lines[-1], i<-1)   
  #if(i==1) cat('Comments removed\n')
  return(lines)
}
#' Remove comments at the end of a string
#' 
#' @param line A string.
#' @return The string, without the commented part.
remove_comments_end_of_line <- function(line)
{
  if(grepl('#',line)) return(substr(line,1,regexpr('#',line)-1))
  else return(line)
}
#' Get comments at the beginning of a vector of strings
#' 
#' @param lines Vector of strings, typically obtained from using \code{scan} with \code{sep='\n'}.
#' @return Vector of strings with the commented lines.
get_comments_from_lines <- function(lines)
{
  i <- 0
  comments <- NULL
  while(i==0) 
  {
    ifelse(substr(lines[1], 1,1)=='#', comments <- append(comments,lines[1]), i<-1)   
    lines <- lines[-1]
  }
  return(comments)
}
#' Calculate a weighted geometric mean
#' 
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @seealso \code{\link{weighted.harmean}}, \code{\link{weighted.mean}}, \code{\link{geomean}}, \code{\link{harmean}} and \code{\link{mean}}
weighted.geomean <- function(x, w, ...)
{
  return(prod(x^w, ...)^(1/sum(w)))
}
#' Calculate a weighted harmonic mean
#' 
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{sum}}
#' @seealso \code{\link{weighted.geomean}}, \code{\link{weighted.mean}}, \code{\link{harmean}} \code{\link{geomean}} and \code{\link{mean}}
weighted.harmean <- function(x, w, ...)
{
  return(sum(w)/(sum(w/x, ...)))
}
#' Calculate a harmonic mean
#' 
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{mean}}
#' @seealso \code{\link{geomean}} and \code{\link{mean}}
harmean <- function(x, ...)
{
  return(1/(mean(1/x, ...)))
}
#' Calculate a geometric mean
#' 
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @seealso \code{\link{harmean}} and \code{\link{mean}}
geomean <- function(x, ...)
{
  return(prod(x, ...)^(1/length(x)))
}
#' Model performance measures
#' 
performance_measures <- function(observations, predictions,print=F)
{
  mse <- MSE(observations, predictions)
  mae <- MAE(observations, predictions)
  me <- ME(observations, predictions)
  r2 <- RSquare(observations, predictions)
  nseff <- NSeff(observations, predictions)
  if(print)
  {
    cat(paste('MSE: ', round(mse,2), ' (Mean Squared Error)\n'))
    cat(paste('MAE: ', round(mae,2), ' (Mean Absolute Error)\n'))
    cat(paste('ME: ', round(me,2), ' (Mean Error)\n'))
    cat(paste('R^2: ', round(r2,2), ' (R squared)\n'))
    cat(paste('NSeff: ',round(nseff,2), ' (Nash-Sutcliffe efficiency)\n'))  
  }
  return(data.frame(mse=mse, mae=mae, me=me, r2=r2, nseff=nseff))
}
#' Reversed rainbow color palette
#' 
#' @export
rev_rainbow <- function(...)
{
  return(rev(rainbow(...)))
}
#' Write modflow array
#' 
#' Internal function used in the write_* functions for writing array datasets
write_modflow_array <- function(modflow_array, file, CNSTNT=1, IPRN=-1, append=TRUE) {

  if(is.null(dim(modflow_array))) {
    if(prod(c(modflow_array)[1] == c(modflow_array))==1) {
      cat(paste('CONSTANT ',CNSTNT * c(modflow_array)[1], '\n', sep=''), file=file, append=append)
    } else {
      cat(paste('INTERNAL ',CNSTNT,' (free) ', IPRN, '\n', sep=''), file=file, append=append)
      cat(paste(paste(modflow_array, collapse=' '), '\n', sep=' '), file=file, append=append)     
    }
  } else if(length(dim(modflow_array))==2) {
    if(prod(c(modflow_array)[1] == c(modflow_array))==1) {
      cat(paste('CONSTANT ',CNSTNT * c(modflow_array)[1], '\n', sep=''), file=file, append=append)
    } else {
      cat(paste('INTERNAL ',CNSTNT,' (free) ', IPRN, '\n', sep=''), file=file, append=append)
      if(dim(modflow_array)[1] == 1) {
        cat(paste0(paste(modflow_array, collapse=' '),'\n'), file=file, append=append)
      } else {
        write.table(modflow_array, file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE) 
      }
    }
  } else {
    for(i in 1:dim(modflow_array)[3])
    {
      if(prod(c(modflow_array[,,i])[1] == c(modflow_array[,,i]))==1) {
        cat(paste('CONSTANT ',CNSTNT * c(modflow_array[,,i])[1], '\n', sep=''), file=file, append=append)
      } else {
        cat(paste('INTERNAL ',CNSTNT,' (free) ', IPRN, '\n', sep=''), file=file, append=append)
        if(dim(modflow_array)[1] == 1) {
          cat(paste0(paste(modflow_array[,,i], collapse=' '),'\n'), file=file, append=append)
        } else write.table(modflow_array[,,i], file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  }
}
#' Write modflow variables
#' 
#' Internal function used in the write_* functions for writing single line datasets
write_modflow_variables <- function(..., file, append=TRUE) {
  cat(paste0(paste(..., sep=' ',collapse=' '), '\n'), file=file, append=append)
}
