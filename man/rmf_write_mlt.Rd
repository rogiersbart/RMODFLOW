% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-mlt.R
\name{rmf_write_mlt}
\alias{rmf_write_mlt}
\title{Write a MODFLOW multiplier file}
\usage{
rmf_write_mlt(
  mlt,
  file = {
    
    cat("Please choose mlt file to overwrite or provide new filename ...\\n")
    
    file.choose()
 },
  iprn = -1,
  ...
)
}
\arguments{
\item{mlt}{an \code{RMODFLOW} mlt object}

\item{file}{filename to write to; typically '*.mlt'}

\item{iprn}{format code for printing arrays in the listing file; defaults to -1 (no printing)}

\item{...}{arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.}
}
\value{
\code{NULL}
}
\description{
\code{rmf_write_mlt} writes an MODFLOW multiplier file based on a \code{RMODFLOW} mlt object
}
\seealso{
\code{\link{rmf_read_mlt}}, \code{\link{rmf_create_mlt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?mult.htm}
}
