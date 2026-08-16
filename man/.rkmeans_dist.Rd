% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rkmeans_dist}
\alias{.rkmeans_dist}
\title{.rkmeans_dist}
\usage{
.rkmeans_dist(x, m)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{m}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}, \code{.rkmeans_huber_centre}, \code{.rkmeans_spatial_median}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rkmeans_dist(V, V)
}
