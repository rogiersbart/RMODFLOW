% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute.R
\name{rmfi_replace_in_vector}
\alias{rmfi_replace_in_vector}
\title{Replace values in a vector with corresponding parameter names}
\usage{
rmfi_replace_in_vector(parnam, parval, new, start = parval)
}
\arguments{
\item{parnam}{Character vector of parameter names from a PVAL file.}

\item{parval}{Vector of values. Can be numeric as in PVAL file, but also
character for \emph{e.g.} the transformation.}

\item{new}{Named numeric vector, or named list of functions and/or numeric
values.}
}
\description{
This function is a helper for processing the arguments of \code{\link[=rmf_execute]{rmf_execute()}},
\code{\link[=rmf_analyze]{rmf_analyze()}} and \code{\link[=rmf_optimize]{rmf_optimize()}} that can be named vectors, a named
lists of functions.
}
