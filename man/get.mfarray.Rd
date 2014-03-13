\name{get.mfarray}
\alias{get.mfarray}
\title{Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function}
\usage{
get.mfarray(mfarray.lines, NROW, NCOL, NLAY)
}
\arguments{
  \item{object}{MODFLOW input file text object, starting
  with the free-format control record}
}
\value{
A list containing the array and the remaining text of the
MODFLOW input file
}
\description{
Get an array specified by a free-format control record from
the text lines analyzed in an \code{\link{RMODFLOW}}
\code{read.*} function
}

