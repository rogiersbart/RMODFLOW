% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{rmf_plot.bud}
\alias{rmf_plot.bud}
\title{Plot a MODFLOW volumetric budget}
\usage{
\method{rmf_plot}{bud}(
  bud,
  dis,
  what = "rates",
  fluxes = "all",
  net = FALSE,
  type = "area",
  timesteps = NULL
)
}
\arguments{
\item{bud}{\code{RMODFLOW} bud object}

\item{dis}{\code{RMODFLOW} dis object}

\item{what}{character; what to plot "rates", "cumulative", "total", "difference" or "discrepancy". Defaults to "rates".}

\item{fluxes}{character; either "all" or a character vector with the flux components to plot. Only used when \code{what} is "rates" or "cumulative"}

\item{net}{logical; if TRUE, it sums the inflows and outflows of the flux component to plot the net fluxes. If FALSE, it will plot both the inflows and outflows. Only used when \code{what} is "rates", "cumulative" or "total".}

\item{type}{character; plot type. Either "bar" or "area"}

\item{timesteps}{integer timesteps index to select from a transient budget. A single negative value will select the last available timestep. Defaults to NULL.}
}
\value{
ggplot2 object
}
\description{
\code{rmf_plot.bud} plots a MODFLOW volumetric budget
}
\details{
any flux components that are zero for the entire simulation are ommited from the plot. This might be the case with constant head cells, since these are always written to the budget even if no constant head cells are specified in the model.
By default, geom_area is used for plotting (type = "area"). If there is only one stress period and this stress period is steady-state however, this will return unstacked bar plots. If type is "bar", geom_col is used and stacked bar plots are returned.
}
