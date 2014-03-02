\name{read.huf}
\alias{read.huf}
\title{Read a MODFLOW hydrogeologic unit flow file}
\usage{
read.huf(file, dis = read.dis(paste(substring(file, 1, nchar(file) - 4),
  ".dis", sep = "")))
}
\arguments{
  \item{file}{Filename; typically *.huf}

  \item{dis}{Corresponding discretization file; typically
  *.dis}
}
\value{
Object of class huf
}
\description{
\code{read.ba6} reads in a MODFLOW hydrogeologic unit flow
file and returns it as an \code{\link{RMODFLOW}} huf
object.
}

