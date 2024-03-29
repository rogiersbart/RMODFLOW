% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-hfb.R
\name{rmf_write_hfb}
\alias{rmf_write_hfb}
\title{Write a MODFLOW horizontal flow barrier file}
\usage{
rmf_write_hfb(
  hfb,
  dis = rmf_read_dis(),
  file = {
    
    cat("Please choose hfb file to overwrite or provide new filename ...\\n")
    
    file.choose()
 },
  ...
)
}
\arguments{
\item{hfb}{an \code{RMODFLOW} hfb object}

\item{dis}{an \code{RMODFLOW} dis object}

\item{file}{filename to write to; typically '*.hfb'}

\item{...}{ignored}
}
\value{
\code{NULL}
}
\description{
\code{rmf_write_hfb} writes a MODFLOW horizontal flow barrier file based on an \code{RMODFLOW} hfb object
}
\seealso{
\code{\link{rmf_read_hfb}}, \code{\link{rmf_create_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}
}
