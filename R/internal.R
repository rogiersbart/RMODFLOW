# internal functions in use ####

#' Convert data frame coordinates to another coordinate reference system
#' 
#' @param dat data frame with x and y coordinates
#' @param from coordinate reference system of the data
#' @param to target coordinate reference system
#' @param names_from names from the data frame coordinate columns
#' @param names_to names to use for the converted coordinates
#' @return data frame with converted coordinates
#' @importFrom sp spTransform SpatialPoints
convert_coordinates <- function(dat, from, to, names_from=c('x','y'), names_to=names_from) {
  nrs <- which(!is.na(dat$x+dat$y))
  dat_names <- names(dat)
  if (length(nrs) > 1) {
    converted_coords <- sp::spTransform(sp::SpatialPoints((cbind(dat[,names_from[1]], dat[,names_from[2]])[nrs, ]), proj4string = from), to)
  } else {
    converted_coords <- sp::spTransform(sp::SpatialPoints(data.frame(cbind(dat[,names_from[1]], dat[,names_from[2]]))[nrs, ], proj4string = from), to)
  } 
  if(length(nrs) == 1) {
    converted_coords <- data.frame(converted_coords$X1,converted_coords$X2)
    
  } else {
    converted_coords <- data.frame(converted_coords$coords.x1,converted_coords$coords.x2)
  }
  names(converted_coords) <- names_to
  if(names_from[1]==names_to[1]) {
    dat[,names_to[1]][nrs] <- converted_coords[,1]
  } else {
    dat$new <- NA
    dat$new[nrs] <- converted_coords[,1]
    names(dat)[ncol(dat)] <- names_to[1]
  }
  if(names_from[2]==names_to[2]) {
    dat[,names_to[2]][nrs] <- converted_coords[,2]
  } else {
    dat$new <- NA
    dat$new[nrs] <- converted_coords[,2]
    names(dat)[ncol(dat)] <- names_to[2]
  }  
  return(dat)
}

#' Calculate a harmonic mean
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{mean}}
#' @seealso \code{\link{geomean}} and \code{\link{mean}}
harmean <- function(x, ...) {
  return(1 / (mean(1 / x, ...)))
}

#' Conditional return
#' 
#' \code{ifelse0} returns \code{yes} if \code{test} is \code{TRUE}. If \code{test} is \code{FALSE}, it returns \code{no}.
#' @param test an object which can be coerced to logical mode.
#' @param yes return value for \code{test==TRUE}
#' @param no return value for \code{test==FALSE}
ifelse0 <- function(test, yes, no) {
  if(test)   {
    return(yes)
  } else {
    return(no)
  }
}

#' Calculate a geometric mean
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @seealso \code{\link{harmean}} and \code{\link{mean}}
geomean <- function(x, ...) {
  return(prod(x, ...) ^ (1 / length(x)))
}

#' Model performance measures
#' @import hydroGOF
performance_measures <- function(observations, predictions,print=F) {
  mse <- mse(observations, predictions)
  mae <- mae(observations, predictions)
  me <- me(observations, predictions)
  r2 <- cor(observations, predictions)^2
  nseff <- NSE(observations, predictions)
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
read_modflow_array <- function(remaining_lines,nrow,ncol,nlay, ndim = NULL) {
  # Initialize array object
  array <- array(dim=c(nrow,ncol,nlay))
  
  # Read array according to format type if there is anything to be read
  if(prod(dim(array))!=0)
  {
    for(k in 1:nlay) 
    { 
      if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1] %in% c('CONSTANT', '0')) {
        if(nlay==1) {
          array[1:length(array)] <- as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])[2])
          remaining_lines <- remaining_lines[-1]
          if(!is.null(ndim)) {
            if(ndim == 1) {
              array <- array(array,dim=nrow*ncol*nlay)
              class(array) <- 'rmodflow_1d_array'
            } else if(ndim == 2) {
              array <- array(array, dim = c(nrow, ncol))
            }
          }
          return(list(array=array,remaining_lines=remaining_lines))
        } else {
          array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])[2]),nrow=nrow,ncol=ncol)
          remaining_lines <- remaining_lines[-1]
        }
      }
      else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1] %in% c('INTERNAL', '100', '103'))
      {
        remaining_lines <- remaining_lines[-1] 
        nPerLine <- length(as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])))
        nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
        array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }
      else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='EXTERNAL')
      {
        stop('Reading EXTERNAL arrays is not implemented yet...')
      }   
      else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='OPEN/CLOSE')
      {
        warning('Reading OPEN/CLOSE arrays is not fully implemented yet...')
        array[,,k] <- as.matrix(read.table(file=remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2]))
        remaining_lines <- remaining_lines[-1] 
      } else {
        # in case of output file arrays without INTERNAL in format line
        nPerNum <- substr(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[9],5,6)
        remaining_lines <- remaining_lines[-1]      
        nPerLine <- length(as.numeric(remove_empty_strings(strsplit((gsub(paste0("([[:alnum:]*[:punct:]*[:space:]]{",nPerNum,"})"), "\\1 ", remaining_lines[1])),' |\t')[[1]])))
        nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
        array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(gsub(paste0("([[:alnum:]*[:punct:]*[:space:]]{",nPerNum,"})"), "\\1 ",paste(remaining_lines[1:nLines],collapse='')),' |\t|\n| \n|\n | \t|\t ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }   
    }
  }
  
  # Set class of object (2darray; 3darray)
  if(is.null(ndim)) {
    if(nlay==1){
      array <- as.matrix(array[,,1])
      class(array) <- 'rmodflow_2d_array'     
    }
    if(nlay!=1) class(array) <- 'rmodflow_3d_array'
  } else if(ndim == 1) {
    array <- array(array,dim=nrow*ncol*nlay)
    class(array) <- 'rmodflow_1d_array'
  } else if(ndim == 2) {
    array <- as.matrix(array[,,1])
    class(array) <- 'rmodflow_2d_array'     
  } else if(ndim == 3) {
    class(array) <- 'rmodflow_3d_array'
  }
  
  # Return output of reading function 
  return(list(array=array,remaining_lines=remaining_lines))
}

#' Read comments
#' Internal function used in the read_* functions to read comments
read_modflow_comments <- function(remaining_lines) {
  i <- 0
  comments <- NULL
  while(i==0) {
    if(substr(remaining_lines[1], 1, 1) == '#') {
      comments <- append(comments, substr(remaining_lines[1], 2, nchar(remaining_lines[1])))
      remaining_lines <- remaining_lines[-1]
    } else {
      i <- 1
    }
  }
  return(list(comments = comments, remaining_lines = remaining_lines))
}

#' Read modflow variables
#' If all are numbers, returns numeric, otherwise returns character vector
read_modflow_variables <- function(remaining_lines) {
  variables <- remove_empty_strings(strsplit(remove_comments_end_of_line(remaining_lines[1]),' |\t')[[1]])
  if(!any(is.na(suppressWarnings(as.numeric(variables))))) variables <- as.numeric(variables)
  return(list(variables=variables,remaining_lines= remaining_lines[-1]))
}

#' Remove comments at the end of a string
#' @param line A string.
#' @return The string, without the commented part.
remove_comments_end_of_line <- function(line) {
  if(grepl('#',line)) return(substr(line,1,regexpr('#',line)-1))
  else return(line)
}

#' Remove empty elements from a vector of strings.
#' @param vector_of_strings Vector of strings.
#' @return Vector of strings without the empty items.
remove_empty_strings <- function(vector_of_strings) {
  return(vector_of_strings[which(vector_of_strings!='')])
}

#' Reversed rainbow color palette
rev_rainbow <- function(...) {
  return(rev(rainbow(...)))
}

#' Calculate a weighted geometric mean
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @seealso \code{\link{weighted.harmean}}, \code{\link{weighted.mean}}, \code{\link{geomean}}, \code{\link{harmean}} and \code{\link{mean}}
weighted.geomean <- function(x, w, ...) {
  return(prod(x^w, ...)^(1/sum(w)))
}

#' Calculate a weighted harmonic mean
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{sum}}
#' @seealso \code{\link{weighted.geomean}}, \code{\link{weighted.mean}}, \code{\link{harmean}} \code{\link{geomean}} and \code{\link{mean}}
weighted.harmean <- function(x, w, ...) {
  return(sum(w)/(sum(w/x, ...)))
}

#' Write modflow array
#' Internal function used in the write_* functions for writing array datasets
write_modflow_array <- function(array, file, cnstnt=1, iprn=-1, append=TRUE) {

  if(is.null(dim(array))) {
    if(prod(c(array)[1] == c(array))==1) {
      cat(paste('CONSTANT ',cnstnt * c(array)[1], '\n', sep=''), file=file, append=append)
    } else {
      cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
      cat(paste(paste(array, collapse=' '), '\n', sep=' '), file=file, append=append)     
    }
  } else if(length(dim(array))==2) {
    if(prod(c(array)[1] == c(array))==1) {
      cat(paste('CONSTANT ',cnstnt * c(array)[1], '\n', sep=''), file=file, append=append)
    } else {
      cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
      if(dim(array)[1] == 1) {
        cat(paste0(paste(array, collapse=' '),'\n'), file=file, append=append)
      } else {
        write.table(array, file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE) 
      }
    }
  } else {
    for(i in 1:dim(array)[3])
    {
      if(prod(c(array[,,i])[1] == c(array[,,i]))==1) {
        cat(paste('CONSTANT ',cnstnt * c(array[,,i])[1], '\n', sep=''), file=file, append=append)
      } else {
        cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
        if(dim(array)[1] == 1) {
          cat(paste0(paste(array[,,i], collapse=' '),'\n'), file=file, append=append)
        } else write.table(array[,,i], file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  }
}

#' Write modflow variables
#' Internal function used in the write_* functions for writing single line datasets
write_modflow_variables <- function(..., file, append=TRUE) {
  cat(paste0(paste(..., sep=' ',collapse=' '), '\n'), file=file, append=append)
}

# internal functions to be replaced by functions above ####

#' Split a string in numbers.
#' @param string A string.
#' @return Vector of numbers within the string, without comments at the end of the string.
split_line_numbers <- function(string) {
  split.line <- as.vector(na.omit(as.numeric(strsplit(remove_comments_end_of_line(string),' |\t')[[1]])))
  return(split.line)
}

#' Split a string in words.
#' @param string A string.
#' @return Vector of words within the string, without comments at the end of the string.
split_line_words <- function(string) {
  split.line <- as.character(strsplit(remove_comments_end_of_line(string),' |\t')[[1]])
  split.line <- remove_empty_strings(split.line)
  return(split.line)
}

#' Get comments at the beginning of a vector of strings
#' @param lines Vector of strings, typically obtained from using \code{scan} with \code{sep='\n'}.
#' @return Vector of strings with the commented lines.
get_comments_from_lines <- function(lines) {
  i <- 0
  comments <- NULL
  while(i==0) 
  {
    ifelse(substr(lines[1], 1,1)=='#', comments <- append(comments,lines[1]), i<-1)   
    lines <- lines[-1]
  }
  return(comments)
}

#' Remove comments at the beginning of a vector of strings
#' @param lines Vector of strings, typically obtained from using \code{scan} with \code{sep='\n'}.
#' @return Vector of strings without the commented lines.
remove_comments_from_lines <- function(lines) {
  i <- 0
  while(i==0) ifelse(substr(lines[1], 1,1)=='#', lines <- lines[-1], i<-1)   
  #if(i==1) cat('Comments removed\n')
  return(lines)
}

