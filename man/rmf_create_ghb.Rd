% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-ghb.R
\name{rmf_create_ghb}
\alias{rmf_create_ghb}
\title{Create an \code{RMODFLOW} ghb object.}
\usage{
rmf_create_ghb(..., dis, ighbcb = 0, noprint = FALSE, aux = NULL)
}
\arguments{
\item{...}{\code{rmf_list} (possibly of class \code{rmf_parameter}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parameter}) elements; defines the general-head boundary cells.}

\item{dis}{dis object}

\item{ighbcb}{flag and unit number for writing cell-by-cell flow terms; defaults to 0 (cell-by-cell flow terms will not be written)}

\item{noprint}{logical, should the printing of GHB cells to the listing file be suppressed ? Defaults to \code{FALSE}}

\item{aux}{optional character vector specifying the names of the auxiliary variables. These variables should also be included in the \code{rmf_list} objects that are supplied; defaults to \code{NULL}}
}
\value{
\code{RMODFLOW} ghb object
}
\description{
\code{rmf_create_ghb} creates an \code{RMODFLOW} ghb object
}
\seealso{
\code{\link{rmf_read_ghb}}, \code{\link{rmf_write_ghb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?ghb.htm}
}
