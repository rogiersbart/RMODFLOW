\name{write.ba6}
\alias{write.ba6}
\title{Write a MODFLOW basic file}
\usage{
write.ba6(ba6, file, info = "No further information provided", IPRN = -1)
}
\arguments{
  \item{ba6}{An \code{\link{RMODFLOW}} ba6 object}

  \item{file}{File to write to; typically "*.ba6"}

  \item{info}{Additional information}

  \item{IPRN}{Needed for the format code.}
}
\value{
None
}
\description{
\code{write.ba6} writes a MODFLOW basic file based on an
\code{\link{RMODFLOW}} ba6 object.
}

