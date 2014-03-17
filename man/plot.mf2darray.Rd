\name{plot.mf2darray}
\alias{plot.mf2darray}
\title{Plot a MODFLOW 2D array}
\usage{
\method{plot}{mf2darray}(mf2darray, dis, ibound = mf2darray * 0 + 1,
  color.palette = terrain.colors, zlim = range(mf2darray, finite = TRUE),
  levels = pretty(zlim, nlevels), nlevels = 20, main = "MF ARRAY plot",
  type = "fill", add = FALSE, xOrigin = 0, yOrigin = 0)
}
\arguments{
  \item{mf2darray}{An object of class mf2darray, or a 2D
  matrix}

  \item{ibound}{An ibound array with 1 or TRUE indicating
  active cells, and 0 or F indicating inactive cells}

  \item{color.palette}{A color palette for imaging the
  parameter values}

  \item{zlim}{}

  \item{levels}{}

  \item{nlevels}{}

  \item{main}{}
}
\value{
None
}
\description{
\code{plot.mf2darray} plots a MODFLOW 2D array.
}

