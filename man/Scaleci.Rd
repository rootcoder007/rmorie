% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Scaleci}
\alias{Scaleci}
\title{Scale-ratio CI from Sukhatme -- eqs. (9.8.1)-(9.8.2), p. 328}
\usage{
Scaleci(x, y, alpha = 0.05, k = NULL)
}
\description{
Scale-ratio CI from Sukhatme -- eqs. (9.8.1)-(9.8.2), p. 328
}
\value{
A list with \code{lower}, \code{upper}, \code{k}, \code{kprime}, \code{k_raw}, \code{npos}, \code{estimate}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Scaleci(V, V)
}
