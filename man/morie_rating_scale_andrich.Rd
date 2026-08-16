% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rating_scale_andrich}
\alias{morie_rating_scale_andrich}
\title{morie_rating_scale_andrich}
\usage{
morie_rating_scale_andrich(theta, b = 0, tau = 0)
}
\description{
morie_rating_scale_andrich
}
\value{
A list with \code{p}, \code{eta}, \code{expected}, \code{info}, \code{theta}, \code{b}, \code{tau}, \code{ncat}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rating_scale_andrich(V)
}
