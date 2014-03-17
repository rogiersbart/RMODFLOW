\name{plot.mf3darray}
\alias{plot.mf3darray}
\title{Plot a layer of a MODFLOW 3D array}
\usage{
\method{plot}{mf3darray}(mf3darray, layer = 1, dis, ibound = mf3darray * 0 +
  1, zlim = range(mf3darray[, , layer], finite = TRUE), ...)
}
\arguments{
  \item{mf3darray}{An object of class mf3darray, or a 3D
  array}

  \item{ibound}{A 3D ibound array with 1 or TRUE indicating
  active cells, and 0 or F indicating inactive cells}

  \item{layer}{The number of the layer to plot}

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
\code{plot.mf3darray} plots a layer of a MODFLOW 3D array.
}

