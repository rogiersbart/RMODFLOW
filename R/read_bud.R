#' Read a MODFLOW budget file
#' 
#' \code{read_bud} reads in a MODFLOW budget file
#' 
#' @param file filename; typically 'Budget.out'
#' @param binary logical; is source file binary?
#' @return object of class bud
#' @importFrom readr read_lines
#' @export
read_bud <- function(file = {cat('Please select bud file ...\n'); file.choose()},
                     binary = TRUE) {
  if(binary) {
    con <- file(file,open='rb')
    bud <- list()
    kstp <- readBin(con,what='integer',n=1)
    kper <- readBin(con,what='integer',n=1)
    desc <- readChar(con,nchars=16)
    while(length(desc!=0)) {
      if(desc=='   CONSTANT HEAD') { name <- 'constant_head'
      } else if(desc=='         STORAGE') { name <- 'storage'
      } else if(desc=='FLOW RIGHT FACE ') { name <- 'flow_right_face'
      } else if(desc=='FLOW FRONT FACE ') { name <- 'flow_front_face'
      } else if(desc=='FLOW LOWER FACE ') { name <- 'flow_lower_face'
      } else if(desc=='           WELLS') { name <- 'wells'
      } else if(desc=='   RIVER LEAKAGE') { name <- 'river_leakage'
      } else if(desc=='        RECHARGE') { name <- 'recharge'
      } else if(desc=='          DRAINS') { name <- 'drains'
      } else if(desc==' HEAD DEP BOUNDS') { name <- 'head_dep_bounds'
      } else {name <- desc}
      
      if(kstp == 1) bud[[name]][[kper]] <- list() # modify for cases where not all time steps are saved!
      bud[[name]][[kper]][[kstp]] <- list()
      bud[[name]][[kper]][[kstp]]$kstp <- kstp
      bud[[name]][[kper]][[kstp]]$kper <- kper
      bud[[name]][[kper]][[kstp]]$desc <- desc
      bud[[name]][[kper]][[kstp]]$ncol <- readBin(con,what='integer',n=1)
      bud[[name]][[kper]][[kstp]]$nrow <- readBin(con,what='integer',n=1)
      bud[[name]][[kper]][[kstp]]$nlay <- readBin(con,what='integer',n=1)
      
      if(bud[[name]][[kper]][[kstp]]$nlay > 0) {
        bud[[name]][[kper]][[kstp]]$data <- aperm(array(readBin(con,what='numeric',n=bud[[name]][[kper]][[kstp]]$ncol*bud[[name]][[kper]][[kstp]]$nrow*bud[[name]][[kper]][[kstp]]$nlay,size=4),dim=c(bud[[name]][[kper]][[kstp]]$ncol,bud[[name]][[kper]][[kstp]]$nrow,bud[[name]][[kper]][[kstp]]$nlay)),c(2,1,3))
      } else {
        bud[[name]][[kper]][[kstp]]$itype <- readBin(con,what='integer',n=1)
        bud[[name]][[kper]][[kstp]]$delt <- readBin(con,what='numeric',n=1,size=4)
        bud[[name]][[kper]][[kstp]]$pertim <- readBin(con,what='numeric',n=1,size=4)
        bud[[name]][[kper]][[kstp]]$totim <- readBin(con,what='numeric',n=1,size=4)
        if(bud[[name]][[kper]][[kstp]]$itype==5) {
          bud[[name]][[kper]][[kstp]]$nval <- readBin(con,what='integer',n=1)
        } else {
          bud[[name]][[kper]][[kstp]]$nval <- 1
        }
        if(bud[[name]][[kper]][[kstp]]$nval > 1) {
          bud[[name]][[kper]][[kstp]]$ctmp <- rep(NA, (bud[[name]][[kper]][[kstp]]$nval-1))
          for(nr in 1:(bud[[name]][[kper]][[kstp]]$nval-1)) {
            bud[[name]][[kper]][[kstp]]$ctmp[nr] <- readChar(con,nchars=16)
          }
        }
        
        if(bud[[name]][[kper]][[kstp]]$itype %in% c(2,5)) {
          bud[[name]][[kper]][[kstp]]$nlist <- readBin(con,what='integer',n=1)
          if(bud[[name]][[kper]][[kstp]]$nlist > 0) {
            bud[[name]][[kper]][[kstp]]$data <- as.data.frame(matrix(NA,nrow=bud[[name]][[kper]][[kstp]]$nlist,ncol=bud[[name]][[kper]][[kstp]]$nval+1))
            names(bud[[name]][[kper]][[kstp]]$data)[1] <- 'icell'
            names(bud[[name]][[kper]][[kstp]]$data)[2] <- 'value'
            # add reading ctmps here!
            for(nr in 1:bud[[name]][[kper]][[kstp]]$nlist) {
              bud[[name]][[kper]][[kstp]]$data[nr,] <- c(readBin(con,what='integer',n=1),readBin(con,what='numeric',n=bud[[name]][[kper]][[kstp]]$nval,size=4))
            }
          }
        }
        if(bud[[name]][[kper]][[kstp]]$itype %in% c(0,1)) {
          bud[[name]][[kper]][[kstp]]$data <- aperm(array(readBin(con,what='numeric',n=bud[[name]][[kper]][[kstp]]$ncol*bud[[name]][[kper]][[kstp]]$nrow*abs(bud[[name]][[kper]][[kstp]]$nlay),size=4),dim=c(bud[[name]][[kper]][[kstp]]$ncol,bud[[name]][[kper]][[kstp]]$nrow,abs(bud[[name]][[kper]][[kstp]]$nlay))),c(2,1,3))
          class(bud[[name]][[kper]][[kstp]]$data) <- 'rmodflow_3d_array'
        }
        if(bud[[name]][[kper]][[kstp]]$itype ==3) {
          bud[[name]][[kper]][[kstp]]$layer <- matrix(readBin(con,what='integer',n=bud[[name]][[kper]][[kstp]]$ncol*bud[[name]][[kper]][[kstp]]$nrow),ncol=bud[[name]][[kper]][[kstp]]$ncol,nrow=bud[[name]][[kper]][[kstp]]$nrow,byrow=TRUE)
          class(bud[[name]][[kper]][[kstp]]$layer) <- 'rmodflow_2d_array'
          bud[[name]][[kper]][[kstp]]$data <- matrix(readBin(con,what='numeric',n=bud[[name]][[kper]][[kstp]]$ncol*bud[[name]][[kper]][[kstp]]$nrow,size=4),ncol=bud[[name]][[kper]][[kstp]]$ncol,nrow=bud[[name]][[kper]][[kstp]]$nrow,byrow=TRUE)
          class(bud[[name]][[kper]][[kstp]]$data) <- 'rmodflow_2d_array'
        }
        if(bud[[name]][[kper]][[kstp]]$itype ==4) {
          bud[[name]][[kper]][[kstp]]$data <- matrix(readBin(con,what='numeric',n=bud[[name]][[kper]][[kstp]]$ncol*bud[[name]][[kper]][[kstp]]$nrow,size=4),ncol=bud[[name]][[kper]][[kstp]]$ncol,nrow=bud[[name]][[kper]][[kstp]]$nrow,byrow=TRUE)
          class(bud[[name]][[kper]][[kstp]]$data) <- 'rmodflow_2d_array'
        }
      }
      
      # set data as the main list item, and include all parameters as attributes
        for(i in 1:(length(bud[[name]][[kper]][[kstp]])-1)) {
          attr(bud[[name]][[kper]][[kstp]]$data,names(bud[[name]][[kper]][[kstp]])[i]) <- bud[[name]][[kper]][[kstp]][[i]]
        }
        bud[[name]][[kper]][[kstp]] <- bud[[name]][[kper]][[kstp]]$data
        
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      desc <- readChar(con,nchars=16)
    }
    
    # create rmodflow_4d_array for list items with itype 0 or 1
      for (i in 1:length(bud)) {
        if (attr(bud[[i]][[1]][[1]],'itype') %in% c(0,1)) {
          bud_item <- bud[[i]]
          bud[[i]] <- unlist(bud_item)
          bud[[i]] <- create_rmodflow_array(bud[[i]], dim = c(attr(bud_item[[1]][[1]],'nrow'), attr(bud_item[[1]][[1]],'ncol'), abs(attr(bud_item[[1]][[1]],'nlay')), length(bud[[i]])/prod(attr(bud_item[[1]][[1]],'nrow'), attr(bud_item[[1]][[1]],'ncol'), abs(attr(bud_item[[1]][[1]],'nlay')))))
          ats <- attributes(bud_item[[length(bud_item)]][[length(bud_item[[length(bud_item)]])]])
          ats <- ats[-which(names(ats) == 'dim')]
          for(at in 1:length(ats)) {
            attr(bud[[i]], names(ats)[at]) <- ats[at][[1]]
          } 
          class(bud[[i]]) <- 'rmodflow_4d_array'
        }
      }
        
    close(con)
    class(bud) <- c('bud','modflow_package')
    return(bud)
  } else {
    stop('Code not up to date')
#     # update this to match the above structure!
#     bud <- list()
#     bud.lines <- read_lines(file)
#     while(length(bud.lines)!=0) {
#       name <- substr(bud.lines[1],25,40)
#       cat('Processing',name,'...\n')    
#       bud[[name]] <- list()
#       bud[[name]]$sp <- as.numeric(substr(bud.lines[1],1,12))
#       bud[[name]]$ts <- as.numeric(substr(bud.lines[1],13,24))
#       bud[[name]]$ncols <- as.numeric(substr(bud.lines[1],41,52))
#       bud[[name]]$nrows <- as.numeric(substr(bud.lines[1],53,64))
#       bud[[name]]$nlays <- as.numeric(substr(bud.lines[1],65,76))
#       bud.lines <- bud.lines[-1]
#       
#       bud[[name]]$code <- as.numeric(substr(bud.lines[1],1,12))
#       bud[[name]]$delt <- as.numeric(substr(bud.lines[1],13,27))
#       bud[[name]]$pertim <- as.numeric(substr(bud.lines[1],28,42))
#       bud[[name]]$totim <- as.numeric(substr(bud.lines[1],43,57))
#       bud.lines <- bud.lines[-1]
#       
#       if(bud[[name]]$code==1) {
#         nrecords <- bud[[name]]$ncols * bud[[name]]$nrows * bud[[name]]$nlays
#         nlines <- ceiling(nrecords/5)
#         # use read_modflow_array or read_modflow_variables instead!!
#         dataVector <- NULL
#         dataVector <- as.numeric(split_line_numbers(paste(bud.lines[1:nlines],collapse=' ')))
#         bud[[name]]$data <- array(dataVector,dim=c(bud[[name]]$ncols,bud[[name]]$nrows,bud[[name]]$nlays))
#         bud[[name]]$data <- aperm(bud[[name]]$data,c(2,1,3))
#         names(bud[[name]]$data) <- c('ID','FLUX')
#         bud.lines <- bud.lines[-c(1:nlines)]
#         bud.lines <- bud.lines[-1]
#       }
#       if(bud[[name]]$code==2) {
#         nrecords <- as.numeric(remove_empty_strings(strsplit(bud.lines[1],' ')[[1]]))
#         bud.lines <- bud.lines[-1]
#         # use read_modflow_array or read_modflow_variables instead!!
#         dataVector <- as.numeric(split_line_numbers(paste(bud.lines[1:nrecords],collapse=' ')))
#         bud[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=2,byrow=T))
#         names(bud[[name]]$data) <- c('ID','FLUX')
#         bud.lines <- bud.lines[-c(1:nrecords)]
#         bud.lines <- bud.lines[-1]
#       }
#       if(bud[[name]]$code==3 | bud[[name]]$code==4) {
#         nrecords <- bud[[name]]$ncols * bud[[name]]$nrows
#         nlines <- ceiling(nrecords/5)
#         dataVector <- NULL
#         # use read_modflow_array or read_modflow_variables instead!!
#         dataVector <- as.numeric(split_line_numbers(paste(bud.lines[1:nlines],collapse=' ')))
#         bud[[name]]$data <- matrix(dataVector,ncol=bud[[name]]$ncols,nrow=bud[[name]]$nrows,byrow=T)
#         bud.lines <- bud.lines[-c(1:nlines)]
#         bud.lines <- bud.lines[-1]
#       }
#       if(bud[[name]]$code==5) {
#         nvalues <- as.numeric(remove_empty_strings(strsplit(bud.lines[1],' ')[[1]]))
#         bud.lines <- bud.lines[-1]
#         if(nvalues > 1) {
#           additionalColumns <- rep(NA,nvalues-1)
#           for(i in 1:(nvalues-1)) {
#             additionalColumns[i] <- remove_empty_strings(strsplit(bud.lines[1],' ')[[1]])
#             bud.lines <- bud.lines[-1]
#           }
#           #bud.lines <- bud.lines[-1] #IFACE
#           #bud.lines <- bud.lines[-1] #CONDFACT
#           #bud.lines <- bud.lines[-1] #CELLGRP
#         }
#         nrecords <- as.numeric(remove_empty_strings(strsplit(bud.lines[1],' ')[[1]]))
#         bud.lines <- bud.lines[-1]
#         # use read_modflow_array or read_modflow_variables instead!!
#         dataVector <- as.numeric(split_line_numbers(paste(bud.lines[1:nrecords],collapse=' ')))
#         bud[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=nvalues+1,byrow=T))
#         if(nvalues > 1) names(bud[[name]]$data) <- c('ID','FLUX',additionalColumns)
#         if(nvalues == 1)names(bud[[name]]$data) <- c('ID','FLUX')
#         bud.lines <- bud.lines[-c(1:nrecords)]
#         bud.lines <- bud.lines[-1]
#       }
#     }
#     class(bud) <- c('bud','modflow_package')
#     return(bud)
   }
}
