\name{read.hed}
\alias{read.hed}
\title{Read a MODFLOW head file}
\usage{
read.hed(file, dis = read.dis(paste(substring(file, 1, nchar(file) - 4),
  ".dis", sep = "")), ba6 = read.ba6(paste(substring(file, 1, nchar(file) -
  4), ".ba6", sep = "")))
}
\arguments{
  \item{file}{Filename; typically "*.hed"}
}
\value{
Object of class hed
}
\description{
\code{read.hed} reads in a MODFLOW head file and returns it
as an \code{\link{RMODFLOW}} hed object.
}

