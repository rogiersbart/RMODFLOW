% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial.R
\name{rmfi_parse_prj}
\alias{rmfi_parse_prj}
\title{Read RMODFLOW projection information from header comments}
\usage{
rmfi_parse_prj(comments)
}
\arguments{
\item{comments}{strings possibly containing RMODFLOW projection information.}
}
\value{
a list with a \code{prj} object and the remaining comments
}
\description{
Read RMODFLOW projection information from header comments
}
\details{
\code{comments} is typically the output of \code{rmfi_parse_comments} as called when reading the discretization file.
RMODFLOW projection is typically present in the header comments of the discretization file.
}
\keyword{internal}
