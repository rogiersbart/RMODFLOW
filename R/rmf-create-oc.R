#' Create an \code{RMODFLOW} oc object
#' 
#' \code{rmf_create_oc} creates an \code{RMODFLOW} oc object.
#' 
#' @param dis dis object
#' @param ihedfm code for the format in which heads will be printed; defaults to NA (heads are not printed)
#' @param chedfm code for the format in which heads will be saved; defaults to NA (heads are saved in binary file)
#' @param ihedun unit number on which heads will be saved; defaults to 666
#' @param iddnfm code for the format in which drawdowns will be printed; defaults to NA (drawdowns are not printed)
#' @param cddnfm code for the format in which drawdowns will be saved; defaults to NA (drawdowns are saved in binary file)
#' @param iddnun unit number on which drawdowns will be saved; defaults to NA (drawdowns are not saved)
#' @param cboufm code for the format in which ibound will be saved; defaults to NA (ibound is saved in binary file)
#' @param ibouun unit number on which ibound will be saved; defaults to NA (ibound is not saved)
#' @param compact_budget logical; should the COMPACT BUDGET option be used
#' @param aux logical; should the auxiliary data be included in the budget file
#' @param head_label logical; should labels be included in the unformatted head file?
#' @param drawdown_label logical; should labels be included in the unformatted drawdown file?
#' @param ibound_label logical; should labels be included in the unformatted ibound file?
#' @param iperoc vector of stress period numbers
#' @param itsoc vector of time step numbers (within the corresponding stress period)
#' @param print_head logical vector; should heads be printed for the time step corresponding to iperoc and itsoc?
#' @param print_drawdown logical vector; should drawdowns be printed for the time step corresponding to iperoc and itsoc?
#' @param print_budget logical vector; should budget be printed for the time step corresponding to iperoc and itsoc?
#' @param save_head logical vector; should heads be saved for the time step corresponding to iperoc and itsoc?
#' @param save_drawdown logical vector; should drawdowns be saved for the time step corresponding to iperoc and itsoc?
#' @param save_ibound logical vector; should ibound be saved for the time step corresponding to iperoc and itsoc?
#' @param save_budget logical vector; should budget be saved for the time step corresponding to iperoc and itsoc?
#' @param incode vector of length \code{sum(dis$nstp)}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param ihddfl vector of length \code{sum(dis$nstp)}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param ibudfl vector of length \code{sum(dis$nstp)}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param icbcfl vector of length \code{sum(dis$nstp)}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param hdpr matrix of dimensions \code{sum(dis$nstp), dis$nlay}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param ddpr matrix of dimensions \code{sum(dis$nstp), dis$nlay}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param hdsv matrix of dimensions \code{sum(dis$nstp), dis$nlay}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param ddsv matrix of dimensions \code{sum(dis$nstp), dis$nlay}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#'
#' @return Object of class oc
#' @export
#' @seealso \code{\link{rmf_read_oc}}, \code{\link{rmf_write_oc}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?oc.htm}
rmf_create_oc <- function(dis = create_dis(),
                          ihedfm = NA,
                          chedfm = NA,
                          ihedun = 666,
                          iddnfm = NA,
                          cddnfm = NA,
                          iddnun = NA,
                          cboufm = NA,
                          ibouun = NA,
                          compact_budget = TRUE,
                          aux = FALSE,
                          head_label = TRUE,
                          drawdown_label = TRUE,
                          ibound_label = TRUE,
                          iperoc = rep(1:dis$nper,dis$nstp),
                          itsoc = unlist(apply(data.frame(1,dis$nstp),1,FUN=function(x) x[1]:x[2])),
                          print_head = rep(FALSE, length(iperoc)),
                          print_drawdown = rep(FALSE, length(iperoc)),
                          print_budget = rep(FALSE, length(iperoc)),
                          save_head = rep(TRUE, length(iperoc)),
                          save_drawdown = rep(FALSE, length(iperoc)),
                          save_ibound = rep(FALSE, length(iperoc)),
                          save_budget = rep(TRUE, length(iperoc)),
                          incode = NULL,
                          ihddfl = NULL,
                          ibudfl = NULL,
                          icbcfl = NULL,
                          hdpr = NULL, 
                          ddpr = NULL,
                          hdsv = NULL,
                          ddsv = NULL
                          ) {
      
  oc <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting oc object
  
    if(is.null(incode)) { # words
      # data set 1
      oc$ihedfm <- ihedfm
      oc$chedfm <- chedfm
      oc$ihedun <- ihedun
      oc$iddnfm <- iddnfm
      oc$cddnfm <- cddnfm
      oc$iddnun <- iddnun
      oc$cboufm <- cboufm
      oc$ibouun <- ibouun
      oc$compact_budget <- compact_budget
      oc$aux <- aux
      oc$head_label <- head_label
      oc$drawdown_label <- drawdown_label
      oc$ibound_label <- ibound_label
      
      # data set 2
      oc$iperoc <- iperoc
      oc$itsoc <- itsoc
      
      # data set 3
      oc$print_head <- print_head
      oc$print_drawdown <- print_drawdown
      oc$print_budget <- print_budget
      oc$save_head <- save_head
      oc$save_drawdown <- save_drawdown
      oc$save_ibound <- save_ibound
      oc$save_budget <- save_budget
      

    } else { # numeric codes
      # data set 1
      oc$ihedfm <- ihedfm
      oc$iddnfm <- iddnfm
      oc$ihedun <- ihedun
      oc$iddnun <- iddnun
      
      # data set 2
      oc$incode <- incode
      oc$ihddfl <- ihddfl
      oc$ibudfl <- ibudfl
      oc$icbcfl <- icbcfl
      
      # data set 3
      oc$hdpr <- hdpr
      oc$ddpr <- ddpr
      oc$hdsv <- hdsv
      oc$ddsv <- ddsv
    }
  class(oc) <- c('oc')
  return(oc)
}

#' @describeIn rmf_create_oc Deprecated function name
#' @export
create_oc <- function(...) {
  .Deprecated(new = "rmf_create_oc", old = "create_oc")
  rmf_create_oc(...)
}
