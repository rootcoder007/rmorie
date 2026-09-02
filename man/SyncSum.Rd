% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SyncSum}
\alias{SyncSum}
\title{Eq (3.96): the sum separates into a signal sum growing linearly in M}
\usage{
SyncSum(observations) 
}
\arguments{
\item{observations}{Iterated over elementwise, with \code{lapply}.}
}
\description{
and a zero-mean noise sum growing only as sqrt(M).  The SUM is returned, because that is the equation; the average is beside it so the division by M is explicit.
}
\value{
A list with \code{sum}, \code{average}, \code{m}, \code{n}, \code{signal_growth}, \code{noise_growth}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SyncSum(V)
}
