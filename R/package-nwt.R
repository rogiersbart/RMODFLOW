#' Create an \code{RMODFLOW} nwt object
#' 
#' \code{rmf_create_nwt} creates an \code{RMODFLOW} nwt object.
#' 
#' @param headtol maximum head change between outer iterations for reaching convergence
#' @param fluxtol maximum root-mean-squared flux difference between outer iterations for reaching convergence
#' @param maxiterout maximum number of outer iterations allowed
#' @param thickfact portion of cell thickness used for smoothly adjusting storage and conductance values to zero
#' @param linmeth flag indicating if the GMRES (1) matrix solver or the XMD (2) matrix solver will be used
#' @param iprnwt flag indicating if information about solver convergence is printed to the listing file
#' @param ibotav logical; indicating whether corrections are made to groundwater head relative to the cell-bottom altitude if the cell is surrounded by dewatered cells.
#' @param options character vector; possible values include \code{"SPECIFIED"}, \code{"SIMPLE"}, \code{"MODERATE"} and \code{"COMPLEX"}. See details for more information.
#' @param continue logical; should the model solve a subsequent time step after it fails to converge ? Defaults to FALSE.
#' @param dbdtheta coefficient used to reduce the weight applied to the head change between nonlinear iterations
#' @param dbdkappa coefficient used to increase the weight applied to the ead change between nonlinear iteraions
#' @param dbdgamma factor used to weight the head change gor iterations \code{n-1} and \code{n}
#' @param momfact momentum coefficient
#' @param backflag flag used to specify whether residual control will be used. A value of 1 indicates residual control is active, a value of 0 indicates it's inactive.  
#' @param maxbackiter is the maximum number of reductions in the head change between nonlinear iterations
#' @param backtol proportional decrease in the root-mean-squared error of the groundwater-flow equation used to determine if residual control is required at the end of a nonlinear iteration
#' @param backreduce reduction factor used for residual control that reduces head change between nonlinear iterations
#' @param maxitinner read if \code{LINMETHD = 1} & \code{options = "SPECIFIED"}; maximum number of iterations for the linear solution
#' @param ilumethod read if \code{LINMETHD = 1} & \code{options = "SPECIFIED"}; index for selection of the method for incomplete factorization (ILU) used as a preconditioner. See details.
#' @param levfill read if \code{LINMETHD = 1} & \code{options = "SPECIFIED"}; fill limit for \code{ILUMETHOD = 1} and level of fill for \code{ILUMETHOD = 2}.
#' @param stoptol read if \code{LINMETHD = 1} & \code{options = "SPECIFIED"}; tolerance for convergence of the linear solver.
#' @param msdr read if \code{LINMETHD = 1} & \code{options = "SPECIFIED"}; number of iterations between restarts of the GMRES solver.
#' @param iacl read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; flag for the acceleration method: 0 = conjugate gradient; 1 = ORTHOMIN, 2 = Bi-CGSTAB
#' @param norder read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; flag for the scheme of ordering the unknowns; 0 = original ordering, 1 = RCM ordering; 2 = Minimum degree ordering
#' @param level read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; level of fill for incomplte LU factorization
#' @param north read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; number of orthogonalization for the ORTHOMIN acceleration scheme. Should equal 2 for the other acceleration methods
#' @param iredsys read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; flag for applying reduced system preconditioning: 1 = apply; 0 = do not apply
#' @param rrctols read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; residual reduction-convergence criteria
#' @param idroptol read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; flag for using drop tolerance in the preconditioning
#' @param epsrn read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; drop tolerance for preconditioning
#' @param hclosexmd read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; head closure criteria for inner (linear) iteraions
#' @param mxiterxmd read if \code{LINMETHD = 2} & \code{options = "SPECIFIED"}; maximum number of iteraions for the linear solution
#' @return Object of class nwt
#' @details the options keyword controls the default values for input arguments \code{dbdtheta - mxiterxmd}. If options = "SPECIFIED", the user specifies the values.
#'   If options = "SIMPLE", default values will be defined by NWT that work well for nearly linear models. If options =" MODERATE", default input values will be defined by NWT that work well for moderately nonlinear models.
#'   If options = "COMPLEX", default values will be defined by NWT that work well for highly nonlinear models. See the MODFLOW-NWT manual for details on these values.
#'   Default values for \code{dbdtheta - mxiterxmd} are set to those used when options = "MODERATE".
#'   
#'   If \code{ILUMETHODE = 1}, ILU with drop tolerance and fill limit is used. If \code{ILUMETHOD = 2}, ILU(k), Order k incomplete LU factorization is used. See the MODFLOW-NWT and Kipp et al. (2008) for details.
#'   
#' @note nwt must be used with the Upstream Weighting flow package. See also \code{\link{rmf_create_upw}}.
#' @references Kipp, K.L., Jr., Hsieh, P.A. & Charlton, S.R. 2008, Guide to the revised groundwater flow and heat transport simulator; HYDROTERM - Version 3: U.S. Geological Survey Techniques and Methods 6-A25, 160 p.
#' @export
#' @seealso \code{\link{rmf_read_nwt}}, \code{\link{rmf_write_nwt}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_create_nwt <- function(headtol = 0.001,
                           fluxtol = 500, 
                           maxiterout = 100,
                           thickfact = 0.00001,
                           linmeth = 2,
                           iprnwt = 0,
                           ibotav = 0,
                           options = 'MODERATE',
                           continue = FALSE,
                           dbdtheta = 0.7,
                           dbdkappa = 0.001,
                           dbdgamma = 0.0,
                           momfact = 0.1,
                           backflag = 0,
                           maxbackiter = 30,
                           backtol = 2,
                           backreduce = 0.99,
                           maxitinner = 50,
                           ilumethod = 2,
                           levfill = 1,
                           stoptol = 1e-10,
                           msdr = 10,
                           iacl = 2,
                           norder = 1,
                           level = 1,
                           north = 2,
                           iredsys = 0,
                           rrctols = 0.0,
                           idroptol = 1,
                           epsrn = 1e-3,
                           hclosexmd = 1e-4,
                           mxiterxmd = 50) {
  nwt <- NULL
  
  # data set 0
  # to provide comments, use ?comment on the resulting nwt object
  
  # data set 1
  nwt$headtol <- headtol
  nwt$fluxtol <- fluxtol
  nwt$maxiterout <- maxiterout
  nwt$thickfact <- thickfact
  nwt$linmeth <- linmeth
  nwt$iprnwt <- iprnwt
  nwt$ibotav <- ibotav
  nwt$options <- toupper(options)
  nwt$continue <- continue
  
  if(nwt$options == "SPECIFIED") {
    nwt$dbdtheta <- dbdtheta
    nwt$dbdkappa <- dbdkappa
    nwt$dbdgamma <- dbdgamma
    nwt$momfact <- momfact
    nwt$backflag <- backflag
    nwt$maxbackiter <- maxbackiter
    nwt$backtol <- backtol
    nwt$backreduce <- backreduce
    
    # data set 2a
    if(nwt$linmeth == 1) {
      nwt$maxitinner <- maxitinner
      nwt$ilumethod <- ilumethod
      nwt$levfill <- levfill
      nwt$stoptol <- stoptol
      nwt$msdr <- msdr
      
    # data set 2b
    } else if(nwt$linmeth == 2) {
      nwt$iacl <- iacl
      nwt$norder <- norder
      nwt$level <- level
      nwt$north <- north
      nwt$iredsys <- iredsys
      nwt$rrctols <- rrctols
      nwt$idroptol <- idroptol
      nwt$epsrn <- epsrn
      nwt$hclosexmd <- hclosexmd
      nwt$mxiterxmd <- mxiterxmd
    }
  }
  
  class(nwt) <- c('nwt','rmf_package')
  return(nwt)
}

#' Read a MODFLOW Newton solver package file
#' 
#' \code{rmf_read_nwt} reads in a MODFLOW Newton solver package file and returns it as an \code{\link{RMODFLOW}} nwt object.
#' 
#' @param file filename; typically '*.nwt'
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @return object of class nwt
#' @export
#' @seealso \code{\link{rmf_write_nwt}}, \code{\link{rmf_create_nwt}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_read_nwt <- function(file = {cat('Please select nwt file ...\n'); file.choose()}, ...) {
  
  nwt_lines <- readr::read_lines(file, lazy = FALSE)
  nwt <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(nwt_lines)
  comment(nwt) <- data_set_0$comments
  nwt_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(nwt_lines, n = 17, ...)
  nwt$headtol <- rmfi_ifelse0(is.na(data_set_1$variables[1]), 0, as.numeric(data_set_1$variables[1]))
  nwt$fluxtol <- rmfi_ifelse0(is.na(data_set_1$variables[2]), 0, as.numeric(data_set_1$variables[2]))
  nwt$maxiterout <- rmfi_ifelse0(is.na(data_set_1$variables[3]), 0, as.numeric(data_set_1$variables[3]))
  nwt$thickfact <- rmfi_ifelse0(is.na(data_set_1$variables[4]), 0, as.numeric(data_set_1$variables[4]))
  nwt$linmeth <- rmfi_ifelse0(is.na(data_set_1$variables[5]), 0, as.numeric(data_set_1$variables[5]))
  nwt$iprnwt <- rmfi_ifelse0(is.na(data_set_1$variables[6]), 0, as.numeric(data_set_1$variables[6]))
  nwt$ibotav <- rmfi_ifelse0(is.na(data_set_1$variables[7]), 0, as.numeric(data_set_1$variables[7]))
  nwt$options <- toupper(as.character(data_set_1$variables[8]))
  
  if((length(data_set_1$variables) > 8) && (toupper(as.character(data_set_1$variables[9])) == 'CONTINUE')) {
    nwt$continue <- TRUE
    data_set_1$variables <- data_set_1$variables[-9]
  } else {
    nwt$continue <- FALSE
  }
  
  if(nwt$options == "SPECIFIED") {
    nwt$dbdtheta <- rmfi_ifelse0(is.na(data_set_1$variables[9]), 0, as.numeric(data_set_1$variables[9]))
    nwt$dbdkappa <- rmfi_ifelse0(is.na(data_set_1$variables[10]), 0, as.numeric(data_set_1$variables[10]))
    nwt$dbdgamma <- rmfi_ifelse0(is.na(data_set_1$variables[11]), 0, as.numeric(data_set_1$variables[11]))
    nwt$momfact <- rmfi_ifelse0(is.na(data_set_1$variables[12]), 0, as.numeric(data_set_1$variables[12]))
    nwt$backflag <- rmfi_ifelse0(is.na(data_set_1$variables[13]), 0, as.numeric(data_set_1$variables[13]))
    nwt$maxbackiter <- rmfi_ifelse0(is.na(data_set_1$variables[14]), 0, as.numeric(data_set_1$variables[14]))
    nwt$backtol <- rmfi_ifelse0(is.na(data_set_1$variables[15]), 0, as.numeric(data_set_1$variables[15]))
    nwt$backreduce <- rmfi_ifelse0(is.na(data_set_1$variables[16]), 0, as.numeric(data_set_1$variables[16]))
    nwt_lines <- data_set_1$remaining_lines
    rm(data_set_1)
    
    
    # data set 2a
    if(nwt$linmeth == 1) {
      data_set_2 <- rmfi_parse_variables(nwt_lines, n = 5, ...)
      
      nwt$maxitinner <- rmfi_ifelse0(is.na(data_set_2$variables[1]), 0, as.numeric(data_set_2$variables[1]))
      nwt$ilumethod <- rmfi_ifelse0(is.na(data_set_2$variables[2]), 0, as.numeric(data_set_2$variables[2]))
      nwt$levfill <- rmfi_ifelse0(is.na(data_set_2$variables[3]), 0, as.numeric(data_set_2$variables[3]))
      nwt$stoptol <- rmfi_ifelse0(is.na(data_set_2$variables[4]), 0, as.numeric(data_set_2$variables[4]))
      nwt$msdr <- rmfi_ifelse0(is.na(data_set_2$variables[5]), 0, as.numeric(data_set_2$variables[5]))
      
      # data set 2b
    } else if(nwt$linmeth == 2) {
      data_set_2 <- rmfi_parse_variables(nwt_lines, n = 10, ...)
      
      nwt$iacl <- rmfi_ifelse0(is.na(data_set_2$variables[1]), 0, as.numeric(data_set_2$variables[1]))
      nwt$norder <- rmfi_ifelse0(is.na(data_set_2$variables[2]), 0, as.numeric(data_set_2$variables[2]))
      nwt$level <- rmfi_ifelse0(is.na(data_set_2$variables[3]), 0, as.numeric(data_set_2$variables[3]))
      nwt$north <- rmfi_ifelse0(is.na(data_set_2$variables[4]), 0, as.numeric(data_set_2$variables[4]))
      nwt$iredsys <- rmfi_ifelse0(is.na(data_set_2$variables[5]), 0, as.numeric(data_set_2$variables[5]))
      nwt$rrctols <- rmfi_ifelse0(is.na(data_set_2$variables[6]), 0, as.numeric(data_set_2$variables[6]))
      nwt$idroptol <- rmfi_ifelse0(is.na(data_set_2$variables[7]), 0, as.numeric(data_set_2$variables[7]))
      nwt$epsrn <- rmfi_ifelse0(is.na(data_set_2$variables[8]), 0, as.numeric(data_set_2$variables[8]))
      nwt$hclosexmd <- rmfi_ifelse0(is.na(data_set_2$variables[9]), 0, as.numeric(data_set_2$variables[9]))
      nwt$mxiterxmd <- rmfi_ifelse0(is.na(data_set_2$variables[10]), 0, as.numeric(data_set_2$variables[10]))
      
    }
    rm(data_set_2)
    
  }
  
  class(nwt) <- c('nwt','rmf_package')
  return(nwt)
}

#' Write a MODFLOW Newton solver package file
#' 
#' @param nwt an \code{\link{RMODFLOW}} nwt object
#' @param file filename to write to; typically '*.nwt'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_nwt}}, \code{\link{rmf_create_nwt}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_write_nwt <- function(nwt,
                          file = {cat('Please select nwt file to overwrite or provide new filename ...\n'); file.choose()}, ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Newton solver package File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(nwt)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(nwt$options != 'SPECIFIED') {
    rmfi_write_variables(nwt$headtol, nwt$fluxtol, as.integer(nwt$maxiterout), nwt$thickfact, as.integer(nwt$linmeth), as.integer(nwt$iprnwt), as.integer(nwt$ibotav), toupper(nwt$options), ifelse(nwt$continue, 'CONTINUE', ''), 
                         file=file, ...)
  } else {
    rmfi_write_variables(nwt$headtol, nwt$fluxtol, as.integer(nwt$maxiterout), nwt$thickfact, as.integer(nwt$linmeth), as.integer(nwt$iprnwt), as.integer(nwt$ibotav), toupper(nwt$options), ifelse(nwt$continue, 'CONTINUE', ''), 
                         nwt$dbdtheta, nwt$dbdkappa, nwt$dbdgamma, nwt$momfact, as.integer(nwt$backflag), as.integer(nwt$maxbackiter), nwt$backtol, nwt$backreduce, file=file, ...)
  }

  # data set 2
  if(toupper(nwt$options) == "SPECIFIED") {
    
    if(nwt$linmeth == 1) {
      rmfi_write_variables(as.integer(nwt$maxitinner), as.integer(nwt$ilumethod), as.integer(nwt$levfill), nwt$stoptol, as.integer(nwt$msdr), file = file, ...)
    } else if(nwt$linmeth == 2) {
      rmfi_write_variables(as.integer(nwt$iacl), as.integer(nwt$norder), as.integer(nwt$level), as.integer(nwt$north), as.integer(nwt$iredsys), nwt$rrctols, as.integer(nwt$idroptol), nwt$epsrn, nwt$hclosexmd, as.integer(nwt$mxiterxmd), file = file, ...)
    }
  }
  
}
