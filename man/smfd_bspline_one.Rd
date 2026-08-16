% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smfd_bspline_one}
\alias{smfd_bspline_one}
\title{Internal: Cox-de Boor recursion for one B-spline. k is 1-based}
\usage{
smfd_bspline_one(x, k, degree, knots)
}
\arguments{
\item{x}{See Usage.}
\item{k}{See Usage.}
\item{degree}{See Usage.}
\item{knots}{See Usage.}
}
\description{
(Python k-1 in 0-based). Mirrors Python .bspline(x, k, degree, knots) in smfd, including the half-open support, the special right-closed last interval, and the strict-positive denominators.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
smfd_bspline_one(x = 5L, k = 3L, degree = 3L, knots = c(1, 2, 3, 4, 5, 6, 7, 8))
}
