% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-dis.R
\name{rmf_write_dis}
\alias{rmf_write_dis}
\title{Write a MODFLOW discretization file}
\usage{
rmf_write_dis(
  dis,
  file = {
    
    cat("Please select dis file to overwrite or provide new filename ...\\n")
    
    file.choose()
 },
  iprn = -1,
  ...
)
}
\arguments{
\item{dis}{an \code{\link{RMODFLOW}} dis object}

\item{file}{filename to write to; typically '*.dis'}

\item{iprn}{format code for printing arrays in the listing file; defaults to -1 (no printing)}

\item{...}{arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.}
}
\value{
\code{NULL}
}
\description{
Write a MODFLOW discretization file
}
\seealso{
\code{\link{rmf_read_dis}}, \code{\link{rmf_create_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
}
