% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.metabd_vec}
\alias{.metabd_vec}
\title{.metabd_vec}
\usage{
.metabd_vec(v)
}
\arguments{
\item{v}{Passed to \code{unlist}.}
}
\description{
A step of the metabd_native implementation. Called by \code{abundance_correlation}, \code{bin_contigs}, \code{composite_distance}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.metabd_vec(V)
}
