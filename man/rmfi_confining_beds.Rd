% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internals.R
\name{rmfi_confining_beds}
\alias{rmfi_confining_beds}
\title{Create sequence of confining bed indicators}
\usage{
rmfi_confining_beds(dis)
}
\arguments{
\item{dis}{\code{RMODFLOW} dis object}
}
\value{
vector of length \code{dis$nlay + confining beds} indicating indicating if the index represents a confining bed
}
\description{
Create sequence of confining bed indicators
}
\details{
When confining beds are present, \code{dis$botm} has \code{dis$nlay + number of confining beds} layers.
This functions returns a logical vector indicating which of those layers is a confining bed. This is useful
when handling calculations with \code{dis$botm}, e.g. calculating thicknesses.
}
\keyword{internal}
