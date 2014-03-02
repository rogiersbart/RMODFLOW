\name{read.bud}
\alias{read.bud}
\title{Read a MODFLOW budget ascii file}
\usage{
read.bud(file)
}
\arguments{
  \item{file}{Filename; typically "Budget.out"}
}
\value{
Object of class bud
}
\description{
\code{read.bud} reads in a MODFLOW budget ascii file
created from the binary file (typically *.bud) using
\code{Read_budget.exe} and returns it as an
\code{\link{RMODFLOW}} bud object.
}

