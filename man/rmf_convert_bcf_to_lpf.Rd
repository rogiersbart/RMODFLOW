% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rmf_convert_bcf_to_lpf}
\alias{rmf_convert_bcf_to_lpf}
\title{Convert a bcf to a lpf object}
\usage{
rmf_convert_bcf_to_lpf(
  bcf,
  dis,
  storagecoefficient = FALSE,
  constantcv = FALSE,
  thickstrt = FALSE,
  nocvcorrection = FALSE,
  novfc = FALSE,
  noparcheck = FALSE,
  ss_value = 1e-05,
  wetdry_value = -0.01,
  vk_bot = NULL
)
}
\arguments{
\item{bcf}{\code{RMODFLOW} bcf object}

\item{dis}{\code{RMODFLOW} dis object}

\item{storagecoefficient}{logical; should STORAGECOEFFICIENT keyword be included?; defaults to FALSE}

\item{constantcv}{logical; should CONSTANTCV keyword be included?; defaults to FALSE}

\item{thickstrt}{logical; should THICKSTRT keyword be included?; defaults to FALSE}

\item{nocvcorrection}{logical; should NOCVCORRECTION keyword be included?; defaults to FALSE}

\item{novfc}{logical; should NOVFC keyword be included?; defaults to FALSE}

\item{noparcheck}{logical; should NOPARCHECK keyword be included?; defaults to FALSE}

\item{ss_value}{numeric; value of specific storage for layers with laycon == 1. Defaults to 1e-5. See details}

\item{wetdry_value}{numeric; value of wetdry for layer with laycon == 2. Defaults to -0.01. See details.}

\item{vk_bot}{2d array with the vertical hydraulic conductivity values in the bottom layer. Defaults to NULL. See details.}
}
\value{
object of class lpf
}
\description{
Convert a bcf to a lpf object
}
\details{
Since vcont is not set for the bottom layer, the vertical conductivity in the bottom layer is assumed to be the same as hk unless a vk_bot argument is supplied.
This affects the calculations of vk in the overlying layers as well.

\if{html}{\out{<div class="sourceCode">}}\preformatted{     When confining beds are present, both vkcb and vk are unknowns. Vkcb is therefore set equal to vk and a warning is thrown.
     
     Layers with laycon = 1 in BCF should represent the upper layer of the model and have unconfined conditions. In this case, BCF needs only a specific yield value.
     When converting to LPF however, a specific storage value is also needed and can not be determined from the bcf object. The \code{ss_value} argument specifies this 
     specific storage value.
     
     Layers with laycon = 2 are converted to convertible layer types in LPF. In BCF, these layers do not have wetdry variables specified. In LPF, convertible layers can have 
     wetdry variables specified. The \code{wetdry_value} argument sets the wetdry values for layers with laycon = 2.
}\if{html}{\out{</div>}}
}
\seealso{
\code{\link{rmf_create_lpf}}, \code{\link{rmf_create_bcf}}, \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm} and \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}
}
