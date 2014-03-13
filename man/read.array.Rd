\name{read.array}
\alias{read.array}
\title{Read an array specified by a free-format control record}
\usage{
read.array(mfarray.lines, NROW, NCOL, NLAY)
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
Read an array specified by a free-format control record
}

