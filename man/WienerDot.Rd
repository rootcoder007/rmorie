% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerDot}
\alias{WienerDot}
\title{Eq. (3.155): the same estimate written as an inner product.  x(n)}
\usage{
WienerDot(w, xvec)
}
\arguments{
\item{w}{See Usage.}
\item{xvec}{See Usage.}
}
\description{
runs BACKWARDS in time, its first entry being the current sample; getting that order wrong reverses the filter without any error being raised, which is why the length check is not the only thing recorded.
}
\value{
A list with \code{d_hat}, \code{order}, \code{vector_is_time_reversed}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
WienerDot(V, V)
}
