\name{read.mlt}
\alias{read.mlt}
\title{Read a MODFLOW multiplier file}
\usage{
read.mlt(file, dis = read.dis(paste(substring(file, 1, nchar(file) - 4),
  ".dis", sep = "")))
}
\arguments{
  \item{file}{Filename; typically "*.mlt"}
}
\value{
Object of class ba6
}
\description{
\code{read.mlt} reads in a MODFLOW multiplier file and
returns it as an \code{\link{RMODFLOW}} mlt object.
}

