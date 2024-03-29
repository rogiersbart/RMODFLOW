% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-nwt.R
\name{rmf_read_nwt}
\alias{rmf_read_nwt}
\title{Read a MODFLOW Newton solver package file}
\usage{
rmf_read_nwt(
  file = {
     cat("Please select nwt file ...\\n")
     file.choose()
 },
  ...
)
}
\arguments{
\item{file}{filename; typically '*.nwt'}

\item{...}{arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.}
}
\value{
object of class nwt
}
\description{
\code{rmf_read_nwt} reads in a MODFLOW Newton solver package file and returns it as an \code{\link{RMODFLOW}} nwt object.
}
\seealso{
\code{\link{rmf_write_nwt}}, \code{\link{rmf_create_nwt}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
}
