% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rmf_calculate_thickness}
\alias{rmf_calculate_thickness}
\title{Calculate layer thicknesses}
\usage{
rmf_calculate_thickness(dis, collapse_cbd = FALSE, only_layers = FALSE)
}
\arguments{
\item{dis}{\code{RMODFLOW} dis object}

\item{collapse_cbd}{logical; should the thickness of an underlying confining bed be added to the overlying layer ? Defaults to FALSE.}

\item{only_layers}{logical; should only the thicknesses of model layers be returned ? Defaults to FALSE}
}
\value{
rmf_3d_array with the layer thicknesses. If collapse_cbd = TRUE or only_layers = TRUE or if there are no confining beds present, there are \code{dis$nlay} layers.
Otherwise there are \code{dis$nlay + number of confining beds} layers
}
\description{
Calculate layer thicknesses
}
\details{
The bottom layer can not have a confining bed below.
When collapse_cbd is TRUE, thicknesses of confining beds are added to their respective overlying layers. The confining beds are then removed.
When only_layers is TRUE, the thicknesses of all layers and confining beds are calculated and only the layers are returned. This is useful to calculate e.g. transmissivities.
By default, thicknesses of confining layers are therefore also included in the returned array.
}
