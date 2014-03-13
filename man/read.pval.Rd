\name{read.pval}
\alias{read.pval}
\title{Read a MODFLOW parameter value file}
\usage{
read.pval(file, read.all = F)
}
\arguments{
  \item{file}{Filename; typically "*.pval"}

  \item{read.all}{A logical value indicating if \code{NP}
  parameters should be read, or the full parameter table
  (only relevant if external codes use the pval file for
  storing additional parameters).}
}
\value{
Object of class pval
}
\description{
\code{read.pval} reads in a MODFLOW parameter value file
and returns it as an \code{\link{RMODFLOW}} pval object.
}

