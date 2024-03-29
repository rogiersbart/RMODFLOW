% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-oc.R
\name{rmf_write_oc}
\alias{rmf_write_oc}
\title{Write a MODFLOW output control option file}
\usage{
rmf_write_oc(
  oc,
  file = {
     cat("Please select oc file to overwrite or provide new filename ...\\n")

        file.choose()
 },
  ...
)
}
\arguments{
\item{oc}{an \code{\link{RMODFLOW}} oc object}

\item{file}{filename to write to; typically '*.oc'}

\item{...}{arguments passed to \code{rmfi_write_variables} when writing a fixed format OC file using numeric codes.}
}
\value{
\code{NULL}
}
\description{
\code{rmf_write_oc} writes a MODFLOW output control option file based on an \code{\link{RMODFLOW}} oc object.
}
\seealso{
\code{\link{rmf_read_oc}}, \code{\link{rmf_create_oc}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?oc.htm}
}
