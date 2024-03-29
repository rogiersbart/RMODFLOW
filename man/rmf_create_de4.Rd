% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-de4.R
\name{rmf_create_de4}
\alias{rmf_create_de4}
\title{Create an \code{RMODFLOW} de4 object.}
\usage{
rmf_create_de4(
  itmx = 100,
  mxup = 0,
  mxlow = 0,
  mxbw = 0,
  ifreq = 3,
  mutd4 = 0,
  accl = 1,
  hclose = 0.01,
  iprd4 = 1
)
}
\arguments{
\item{itmx}{the maximum number of iterations in each time step; defaults to 100}

\item{mxup}{the maximum number of equations in the upper part of the equations to be solved; defaults to 0}

\item{mxlow}{the maximum number of equations in the lower part of the equations to be solved; defaults to 0}

\item{mxbw}{the maximum band width plus 1 of the AL matrix; defaults to 0}

\item{ifreq}{a flag indicating the frequency at which the coefficients in A change; defaults to 3}

\item{mutd4}{a flag indicating the quantity of convergence information that is printed during a time step; defaults to 0}

\item{accl}{a multiplier for the computed head change for each iteration; defaults to 1}

\item{hclose}{the head change closure criterion in the unit of length; defaults to 0.01}

\item{iprd4}{the time step interval for printing out convergence information when iterating; defaults to 1}
}
\value{
\code{RMODFLOW} de4 object
}
\description{
\code{rmf_create_de4} creates an \code{RMODFLOW} de4 object
}
\seealso{
\code{\link{rmf_read_de4}}, \code{\link{rmf_write_de4}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?de4.htm}
}
