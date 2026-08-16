% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EnsAvg}
\alias{EnsAvg}
\title{Eq (3.18): x_bar(t) = (1/M) sum_k x_k(t) for all t -- the prototype}
\usage{
EnsAvg(observations)
}
\arguments{
\item{observations}{Iterated over elementwise, with \code{lapply}.}
}
\description{
signal, a filtered version of the M observations.
}
\value{
A list with \code{average}, \code{sd}, \code{m}, \code{n}, \code{se}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
EnsAvg(V)
}
