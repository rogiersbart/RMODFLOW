% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rmf_convert_hed_to_water_table}
\alias{rmf_convert_hed_to_water_table}
\title{Obtain the water table elevation from a hydraulic head array}
\usage{
rmf_convert_hed_to_water_table(hed, l = NULL, na_values = NULL)
}
\arguments{
\item{hed}{2d, 3d or 4d array with hydraulic heads}

\item{l}{integer used to subset the 4th dimension of \code{hed}. If not supplied, the final time step is used}

\item{na_values}{optional; specifies which \code{hed} values should be set to \code{NA}}
}
\value{
\code{rmf_2d_array} with the elevation of the water table, i.e. the first non-NA value in every vertical column of the grid
}
\description{
Obtain the water table elevation from a hydraulic head array
}
