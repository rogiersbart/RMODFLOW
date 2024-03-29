% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-chd.R
\name{rmf_read_chd}
\alias{rmf_read_chd}
\title{Read a MODFLOW time-variant specified-head file}
\usage{
rmf_read_chd(
  file = {
     cat("Please select time-variant specified-head file ...\\n")
    
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
\item{file}{filename; typically '*.chd'}

\item{dis}{an \code{RMODFLOW} dis object}

\item{...}{arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.}
}
\value{
\code{RMODFLOW} chd object
}
\description{
\code{rmf_read_chd} reads in a MODFLOW time-variant specified-head file and returns it as an \code{RMODFLOW} chd object.
}
\seealso{
\code{\link{rmf_write_chd}}, \code{\link{rmf_create_chd}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?chd.htm}
}
