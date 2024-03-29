% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-upw.R
\name{rmf_read_upw}
\alias{rmf_read_upw}
\title{Read a MODFLOW-NWT Upstream Weighting file}
\usage{
rmf_read_upw(
  file = {
     cat("Please select upw file ...\\n")
     file.choose()
 },
  dis = {
     cat("Please select corresponding dis file ...\\n")
    
    rmf_read_dis(file.choose())
 },
  mlt = NULL,
  zon = NULL,
  ...
)
}
\arguments{
\item{file}{filename; typically '*.upw'}

\item{dis}{an \code{RMODFLOW} dis object}

\item{mlt}{a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays}

\item{zon}{a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays}

\item{...}{arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are 'free' format and INTERNAL or CONSTANT.}
}
\value{
object of class upw
}
\description{
\code{rmf_read_upw} reads in a MODFLOW-NWT upstream weighting file and returns it as an \code{\link{RMODFLOW}} upw object.
}
\note{
upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.

upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
}
\seealso{
\code{\link{rmf_write_upw}}, \code{\link{rmf_create_upw}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
}
