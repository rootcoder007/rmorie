% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scintg_l2_normalise}
\alias{.scintg_l2_normalise}
\title{.scintg_l2_normalise}
\usage{
.scintg_l2_normalise(Z)
}
\arguments{
\item{Z}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the scintg_native implementation. Called by \code{.scintg_kmeans_init}, \code{.scintg_maximum_diversity_clustering}. See the file header for the source the module follows.
}
\value{
The value of \code{sweep}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.scintg_l2_normalise(M)
}
