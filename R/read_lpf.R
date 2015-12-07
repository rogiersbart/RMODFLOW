#' Read a MODFLOW layer-property flow file
#' 
#' \code{read_lpf} reads in a MODFLOW layer property file and returns it as an \code{\link{RMODFLOW}} lpf object.
#' 
#' @param file filename; typically '*.lpf'
#' @return object of class lpf
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{write_lpf}}, \code{\link{create_lpf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm}
read_lpf <- function(file = {cat('Please select lpf file...\n'); file.choose()}) {
  
  lpf.lines <- read_lines(file)
  lpf <- NULL
  
  # data set 0
    comments <- get_comments_from_lines(lpf.lines)
    lpf.lines <- remove_comments_from_lines(lpf.lines)
  
  # data set 1
    dataSet1 <- remove_empty_strings(strsplit(lpf.lines[1],' ')[[1]])
    lpf.lines <- lpf.lines[-1]  
    lpf$ILPFCB <- as.numeric(dataSet1[1])
    lpf$HDRY <- as.numeric(dataSet1[2])
    lpf$NPLPF <- as.numeric(dataSet1[3])
    lpf$STORAGECOEFFICIENT <- 'STORAGECOEFFICIENT' %in% dataSet1
    lpf$CONSTANTCV <- 'CONSTANTCV' %in% dataSet1
    lpf$THICKSTRT <- 'THICKSTRT' %in% dataSet1
    lpf$NOCVCORRECTION <- 'NOCVCORRECTION' %in% dataSet1
    lpf$NOVFC <- 'NOVFC' %in% dataSet1
    lpf$NOPARCHECK <- 'NOPARCHECK' %in% dataSet1
    rm(dataSet1)
  
  # data set 2
    lpf$LAYTYP <- as.numeric(remove_empty_strings(strsplit(lpf.lines[1],' ')[[1]]))
    lpf.lines <- lpf.lines[-1]
  
  # data set 3
    lpf$LAYAVG <- as.numeric(remove_empty_strings(strsplit(lpf.lines[1],' ')[[1]]))
    lpf.lines <- lpf.lines[-1]
  
  # data set 4
    lpf$CHANI <- as.numeric(remove_empty_strings(strsplit(lpf.lines[1],' ')[[1]]))
    lpf.lines <- lpf.lines[-1]
  
  # data set 5
    lpf$LAYVKA <- as.numeric(remove_empty_strings(strsplit(lpf.lines[1],' ')[[1]]))
    lpf.lines <- lpf.lines[-1]
    
  # data set 6
    lpf$LAYWET <- as.numeric(remove_empty_strings(strsplit(lpf.lines[1],' ')[[1]]))
    lpf.lines <- lpf.lines[-1]
  
  # data set 7
    if(!as.logical(prod(lpf$LAYWET==0))) {
      dataSet7 <- remove_empty_strings(strsplit(lpf.lines[1],' ')[[1]])
      lpf.lines <- lpf.lines[-1]  
      lpf$WETFCT <- as.numeric(dataSet7[1])
      lpf$IWETIT <- as.numeric(dataSet7[2])
      lpf$IHDWET <- as.numeric(dataSet7[3])
      rm(dataSet7)
    }
  
  # data set 8-9
    lpf$PARNAM <- vector(mode='character',length=lpf$NPLPF)
    lpf$PARTYP <- vector(mode='character',length=lpf$NPLPF)
    lpf$Parval <- vector(mode='numeric',length=lpf$NPLPF)
    lpf$NCLU <- vector(mode='numeric',length=lpf$NPLPF)
    lpf$Mltarr <- matrix(nrow=dis$NLAY, ncol=lpf$NPLPF)
    lpf$Zonarr <- matrix(nrow=dis$NLAY, ncol=lpf$NPLPF)
    lpf$IZ <- matrix(nrow=dis$NLAY, ncol=lpf$NPLPF)
    for(i in 1:lpf$NPLPF) {
      line.split <- split_line_words(lpf.lines[1]); lpf.lines <- lpf.lines[-1]
      lpf$PARNAM[i] <- line.split[1]
      lpf$PARTYP[i] <- line.split[2]
      lpf$Parval[i] <- as.numeric(line.split[3])
      lpf$NCLU[i] <- as.numeric(line.split[4])
      for(j in 1:lpf$NCLU[i]) {
        line.split <- split_line_words(lpf.lines[1]); lpf.lines <- lpf.lines[-1]
        k <- as.numeric(line.split[1])
        lpf$Mltarr[k,i] <- line.split[2]
        lpf$Zonarr[k,i] <- line.split[3]
        lpf$IZ[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
      } 
    }
  
  # data set 10-16
    lpf$HK <- array(dim=c(dis$NROW, dis$NCOL, dis$NLAY))
    class(lpf$HK) <- '3d_array'
    lpf$HANI <- lpf$VKA <- lpf$Ss <- lpf$Sy <- lpf$VKCB <- lpf$WETDRY <- lpf$HK
    for(k in 1:dis$NLAY) {
      
      # data set 10
        if('HK' %in% lpf$PARTYP) {
          lpf.lines <- lpf.lines[-1]  
          lpf$HK[,,k] <- NA
        } else {
          dataSet10 <- read_array(lpf.lines,dis$NROW,dis$NCOL,1)
          lpf.lines <- dataSet10$remaining_lines
          lpf$HK[,,k] <- dataSet10$modflow_array
          rm(dataSet10)
        }
        
      # data set 11
        if(lpf$CHANI[k] <= 0) {
          if('HANI' %in% lpf$PARTYP) {
            lpf.lines <- lpf.lines[-1]  
            lpf$HANI[,,k] <- NA
          } else {
            dataSet11 <- read_array(lpf.lines,dis$NROW,dis$NCOL,1)
            lpf.lines <- dataSet11$remaining_lines
            lpf$HANI[,,k] <- dataSet11$modflow_array
            rm(dataSet11)
          }
        }
        
      # data set 12
        if('VK' %in% lpf$PARTYP | 'VANI' %in% lpf$PARTYP) {
          lpf.lines <- lpf.lines[-1]  
          lpf$VKA[,,k] <- NA
        } else {
          dataSet12 <- read_array(lpf.lines,dis$NROW,dis$NCOL,1)
          lpf.lines <- dataSet12$remaining_lines
          lpf$VKA[,,k] <- dataSet12$modflow_array
          rm(dataSet12)
        }
        
      # data set 13
        if('TS' %in% dis$SSTR) {
          if('SS' %in% lpf$PARTYP) {
            lpf.lines <- lpf.lines[-1]  
            lpf$Ss[,,k] <- NA
          } else {
            dataSet13 <- read_array(lpf.lines,dis$NROW,dis$NCOL,1)
            lpf.lines <- dataSet13$remaining_lines
            lpf$Ss[,,k] <- dataSet13$modflow_array
            rm(dataSet13)
          }
        }
        
      # data set 14
        if('TS' %in% dis$SSTR & lpf$LAYTYP[k] != 0) {
          if('SY' %in% lpf$PARTYP) {
            lpf.lines <- lpf.lines[-1]  
            lpf$Sy[,,k] <- NA
          } else {
            dataSet14 <- read_array(lpf.lines,dis$NROW,dis$NCOL,1)
            lpf.lines <- dataSet14$remaining_lines
            lpf$Sy[,,k] <- dataSet14$modflow_array
            rm(dataSet14)
          }
        }
        
      # data set 15
        if(dis$LAYCBD[k] != 0) {
          if('VKCB' %in% lpf$PARTYP) {
            lpf.lines <- lpf.lines[-1]  
            lpf$VKCB[,,k] <- NA
          } else {
            dataSet15 <- read_array(lpf.lines,dis$NROW,dis$NCOL,1)
            lpf.lines <- dataSet15$remaining_lines
            lpf$VKCB[,,k] <- dataSet15$modflow_array
            rm(dataSet15)
          }
        }
        
      # data set 16
        if(lpf$LAYWET[k] != 0 & lpf$LAYTYP[k] != 0) {
          dataSet16 <- read_array(lpf.lines,dis$NROW,dis$NCOL,1)
          lpf.lines <- dataSet16$remaining_lines
          lpf$WETDRY[,,k] <- dataSet16$modflow_array
          rm(dataSet16)
        }     
    }
  
  comment(lpf) <- comments
  class(lpf) <- c('lpf','modflow_package')
  return(lpf)
}
