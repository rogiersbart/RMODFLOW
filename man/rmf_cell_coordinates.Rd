% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rmf_cell_coordinates}
\alias{rmf_cell_coordinates}
\alias{rmf_cell_coordinates.dis}
\alias{rmf_cell_coordinates.huf}
\alias{rmf_get_cell_coordinates}
\alias{rmf_get_cell_coordinates.dis}
\alias{rmf_get_cell_coordinates.huf}
\title{Generic functions to get cell coordinates}
\usage{
rmf_cell_coordinates(...)

\method{rmf_cell_coordinates}{dis}(dis, prj = rmf_get_prj(dis), include_faces = FALSE)

\method{rmf_cell_coordinates}{huf}(
  huf,
  dis = NULL,
  prj = rmf_get_prj(dis),
  include_faces = FALSE
)

rmf_get_cell_coordinates(...)

\method{rmf_get_cell_coordinates}{dis}(dis, i, j, k, ...)

\method{rmf_get_cell_coordinates}{huf}(huf, i, j, k, ...)
}
\arguments{
\item{...}{additional arguments passed to \code{rmf_cell_coordinates}}

\item{dis}{dis object, corresponding to the huf object}

\item{prj}{projection file object}

\item{include_faces}{logical; should face coordinates be included?}

\item{huf}{huf object}

\item{i}{row indices of required cell(s)}

\item{j}{column indices of required cell(s)}

\item{k}{layer indices of required cell(s)}
}
\value{
\code{rmf_cell_coordinates} returns a list with with cell center x y and z coordinate as 3d arrays and optionally, the face coordinates of all cells

3d array with cell coordinates

\code{rmf_get_cell_coordinates} returns a data frame with the coordinates of specified cells
}
\description{
Generic functions to get cell coordinates
}
