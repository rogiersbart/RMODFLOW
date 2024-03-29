% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-bas.R
\name{rmf_read_bas}
\alias{rmf_read_bas}
\title{Read a MODFLOW basic file}
\usage{
rmf_read_bas(
  file = {
     cat("Please select bas file ...\\n")
     file.choose()
 },
  dis = {
     cat("Please select corresponding dis file ...\\n")
    
    rmf_read_dis(file.choose())
 },
  ...
)
}
\arguments{
\item{file}{filename; typically '*.bas'}

\item{dis}{discretization file object; defaults to that with the same filename but with extension '.dis'}

\item{...}{arguments passed to \code{rmfi_parse_array} and \code{rmfi_parse_variables}. Can be ignored when input is 'free' format and input arrays are INTERNAL or CONSTANT.}
}
\value{
object of class bas
}
\description{
\code{rmf_read_bas} reads in a MODFLOW basic file and returns it as an \code{\link{RMODFLOW}} bas object.
}
\seealso{
\code{\link{rmf_write_bas}}, \code{\link{rmf_create_bas}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?bas6.htm}
}
