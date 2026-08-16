% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EvenOdd}
\alias{EvenOdd}
\title{Eqs (3.92)-(3.94).  Eq (3.94) is an identity, so the reconstruction}
\usage{
EvenOdd(x, n = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
error checks the index bookkeeping, not the arithmetic.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
EvenOdd(S)
}
