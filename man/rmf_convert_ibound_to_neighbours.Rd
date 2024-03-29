% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rmf_convert_ibound_to_neighbours}
\alias{rmf_convert_ibound_to_neighbours}
\title{Convert an \code{ibound} array to lower, upper, left, right, front and back logical arrays indicating presence of a neighbouring active cell}
\usage{
rmf_convert_ibound_to_neighbours(ibound)
}
\arguments{
\item{ibound}{3d \code{ibound} array as specified in a MODFLOW BAS object}
}
\value{
list of lower, upper, left, right, front and back logical 3d arrays
}
\description{
Convert an \code{ibound} array to lower, upper, left, right, front and back logical arrays indicating presence of a neighbouring active cell
}
