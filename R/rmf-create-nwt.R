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
#' @details the option keyword controls the default values for input arguments \code{dbdtheta - mxiterxmd}. If option = "SPECIFIED", the user specifies the values.
#'   If option = "SIMPLE", default values will be defined by NWT that work well for nearly linear models. If option =" MODERATE", default input values will be defined by NWT that work well for moderately nonlinear models.
#'   If option = "COMPLEX", default values will be defined by NWT that work well for highly nonlinear models. See the MODFLOW-NWT manual for details on these values.
#'   Default values for \code{dbdtheta - mxiterxmd} are set to those used when option = "MODERATE".
#'   
#'   If \code{ILUMETHODE = 1}, ILU with drop toleance and fill limit is used. If \code{ILUMETHOD = 2}, ILU(k), Order k incomplete LU factorization is used. See the MODFLOW-NWT and Kipp et al. (2008) for details.
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

