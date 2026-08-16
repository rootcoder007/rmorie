% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rkmeans_mean}
\alias{.rkmeans_mean}
\title{.rkmeans_mean}
\usage{
.rkmeans_mean(pts)
}
\arguments{
\item{pts}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}, \code{.rkmeans_huber_centre}, \code{.rkmeans_spatial_median}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
