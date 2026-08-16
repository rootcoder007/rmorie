% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_nominal_response_bock}
\alias{morie_nominal_response_bock}
\title{morie_nominal_response_bock}
\usage{
morie_nominal_response_bock(theta, a_k = c(0, 1), c_k = c(0, 0))
}
\description{
morie_nominal_response_bock
}
\value{
A list with \code{p}, \code{eta}, \code{expected}, \code{info}, \code{theta}, \code{a_k}, \code{c_k}, \code{ncat}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_nominal_response_bock(V)
}
