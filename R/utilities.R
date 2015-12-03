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
#' Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param object MODFLOW input file text object, starting with the free-format control record
#' @return A list containing the array and the remaining text of the MODFLOW input file
read_modflow_array <- function(remaining_lines,NROW,NCOL,NLAY)
{
  # Initialize array object
  modflow_array <- array(dim=c(NROW,NCOL,NLAY))
  
  # Read array according to format type if there is anything to be read
  if(prod(dim(modflow_array))!=0)
  {
    for(k in 1:NLAY) 
    { 
      # Read in first row with format code
      # If constant and NLAY==1, return constant
      # If constant and NLAY!=1, fill layer with constant (is part of mf3darray!?)
      if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='CONSTANT') 
      {
        if(NLAY==1)
        {
          modflow_array <- as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])[2])
          remaining_lines <- remaining_lines[-1]
          return(list(modflow_array=modflow_array,remaining_lines=remaining_lines))
        } else {
          modflow_array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])[2]),nrow=NROW,ncol=NCOL)
          remaining_lines <- remaining_lines[-1]
        }
      }
      else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='INTERNAL')
      {
        remaining_lines <- remaining_lines[-1] 
        nPerLine <- length(as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])))
        nLines <- (NCOL %/% nPerLine + ifelse((NCOL %% nPerLine)==0, 0, 1))*NROW
        modflow_array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]])),nrow=NROW,ncol=NCOL,byrow=TRUE)
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }
      else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='EXTERNAL')
      {
        stop('Reading EXTERNAL arrays is not implemented yet...')
      }   
      else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='OPEN/CLOSE')
      {
        warning('Reading OPEN/CLOSE arrays is not fully implemented yet...')
        modflow_array[,,k] <- as.matrix(read.table(file=remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2]))
        remaining_lines <- remaining_lines[-1] 
      } else {
        # in case of output file arrays without INTERNAL in format line
        nPerNum <- substr(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[9],5,6)
        remaining_lines <- remaining_lines[-1]      
        nPerLine <- length(as.numeric(remove_empty_strings(strsplit((gsub(paste0("([[:alnum:]*[:punct:]*[:space:]]{",nPerNum,"})"), "\\1 ", remaining_lines[1])),' |\t')[[1]])))
        nLines <- (NCOL %/% nPerLine + ifelse((NCOL %% nPerLine)==0, 0, 1))*NROW
        modflow_array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(gsub(paste0("([[:alnum:]*[:punct:]*[:space:]]{",nPerNum,"})"), "\\1 ",paste(remaining_lines[1:nLines],collapse='')),' |\t|\n| \n|\n | \t|\t ')[[1]])),nrow=NROW,ncol=NCOL,byrow=TRUE)
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }   
    }
  }
  
  # Set class of object (2darray; 3darray)
  if(NLAY==1){modflow_array <- as.matrix(modflow_array[,,1]); class(modflow_array) <- 'modflow_2d_array'}
  if(NLAY!=1) class(modflow_array) <- 'modflow_3d_array'
  
  # Return output of reading function 
  return(list(modflow_array=modflow_array,remaining_lines=remaining_lines))
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
