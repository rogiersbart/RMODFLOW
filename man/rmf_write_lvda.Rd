% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-huf.R
\name{rmf_write_lvda}
\alias{rmf_write_lvda}
\title{Write a MODFLOW model-layer variable-direction horizontal anisotropy capability file}
\usage{
rmf_write_lvda(
  lvda,
  file = {
    
    cat("Please select lvda file to overwrite or provide new filename ...\\n")
    
    file.choose()
 },
  ...
)
}
\arguments{
\item{lvda}{an \code{RMODFLOW} lvda object}

\item{file}{filename to write to; typically '*.lvda'}

\item{...}{ignored}
}
\value{
\code{NULL}
}
\description{
Write a MODFLOW model-layer variable-direction horizontal anisotropy capability file
}
\seealso{
\code{\link{rmf_create_lvda}}, \code{\link{rmf_read_lvda}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lvda.htm}
}
