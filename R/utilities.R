################################################################################
### Utilities ##################################################################
################################################################################

# Remove beginning comment lines.
remove.comments.from.lines <- function(lines)
{
    i <- 0
    while(i==0) ifelse(substr(lines[1], 1,1)=='#', lines <- lines[-1], i<-1)   
    #if(i==1) cat('Comments removed\n')
    return(lines)
}
# Remove comments at the end of a line
remove.comments.end.of.line <- function(line)
{
  if(grepl('#',line)) return(substr(line,1,regexpr('#',line)-1))
  else return(line)
}

# Remove empty strings from string array
remove.empty.strings <- function(stringArray)
{
    newStringArray <- NULL
    for(i in 1:length(stringArray)) 
    {
        #print(stringArray[i])
        if(stringArray[i] != '') {newStringArray <- c(newStringArray, stringArray[i])}
    }
    return(newStringArray)
}

# Split line into characters
split.line.char <- function(string)
{
  split.line <- as.character(strsplit(remove.comments.end.of.line(string),' |\t')[[1]])
  split.line <- remove.empty.strings(split.line)
  return(split.line)
}

# Split line into numbers
split.line.num <- function(string)
{
  split.line <- as.vector(na.omit(as.numeric(strsplit(remove.comments.end.of.line(string),' |\t')[[1]])))
  return(split.line)
}
# Mirror matrix
mirror.matrix <- function(mat, mirror.type='both')
{
  nr <- length(mat[,1])
  nc <- length(mat[1,])
  matmirror <- matrix(nrow=nr, ncol=nc)
  
  if(mirror.type=='both')
  {
    matmirror2 <- matrix(nrow=nr,ncol=nc)
    for(j in 1:nc) matmirror2[,j] <- mat[,nc-j+1]
    for(i in 1:nr) matmirror[i,] <- matmirror2[nr-i+1,]
  }
  if(mirror.type=='horizontal')
  {
    for(j in 1:nc) matmirror[,j] <- mat[,nc-j+1]
  }
  if(mirror.type=='vertical')
  {
    for(i in 1:nr) matmirror[i,] <- mat[(nr-i+1),]
  }
  if(mirror.type!='both' & mirror.type!='horizontal' & mirror.type!='vertical')
  {
    cat('Error: wrong mirror type specified.')
  }
  return(matmirror)
}
# RMSE
rmse <- function(observations, predictions)
{
  return(sqrt(mean((observations-predictions)^2)))
}



