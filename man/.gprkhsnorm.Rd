% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gprkhsnorm}
\alias{.gprkhsnorm}
\title{Chapter 8: reproducing kernel Hilbert space, eqs (8.1)-(8.3)}
\usage{
.gprkhsnorm(beta, K)
}
\arguments{
\item{beta}{Passed to \code{.gpflat}.}
\item{K}{Passed to \code{.gpmat}.}
}
\description{
--------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
.gprkhsnorm(beta = 0.5, K = 5L)
}
