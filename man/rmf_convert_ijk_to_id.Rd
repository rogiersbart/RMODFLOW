% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rmf_convert_ijk_to_id}
\alias{rmf_convert_ijk_to_id}
\title{Convert ijk to id}
\usage{
rmf_convert_ijk_to_id(i, j, k, dis, type = "r")
}
\arguments{
\item{i}{vector of row numbers}

\item{j}{vector of column numbers}

\item{k}{vector of layer numbers}

\item{dis}{a discretization file object}

\item{type}{'r' or 'modflow' specifying type of id. See details. Defaults to 'r'}
}
\value{
cell ids, providing the place of the cell in an input file 3d array
}
\description{
Convert ijk to id
}
\details{
a modflow id provides the place of the number in an input file 3d array (not like the way R uses ids for arrays or matrices; rows and columns are switched)
}
