% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PvcLinDf}
\alias{PvcLinDf}
\title{Section 10.11.1, eq (10.131).  A PVC has both a shorter preceding RR}
\usage{
PvcLinDf(rr, ff, train = NULL) 
}
\arguments{
\item{rr}{A vector; its length is taken.}
\item{ff}{A vector; its length is taken.}
\item{train}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
interval and a more complex waveshape, and the form factor of eq (5.26) turns the qualitative half of the clinical rule into a number.
}
\value{
A list with \code{labels}, \code{discriminant}, \code{coefficients}, \code{prototypes}, \code{source}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PvcLinDf(V, V)
}
