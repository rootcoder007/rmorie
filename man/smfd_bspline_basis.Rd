% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smfd_bspline_basis}
\alias{smfd_bspline_basis}
\title{Internal: the n x p B-spline design matrix. Mirrors Python}
\usage{
smfd_bspline_basis(x, knots, degree = 3L)
}
\arguments{
\item{x}{See Usage.}
\item{knots}{See Usage.}
\item{degree}{Defaults to \code{3L}.}
}
\description{
bspline_basis(x, knots, degree) in smfd.
}
\value{
The value of \code{B}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
smfd_bspline_basis(V, V)
}
