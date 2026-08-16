% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_cross_dist}
\alias{.sp_cross_dist}
\title{Internal: pairwise Euclidean distances between two coordinate sets.}
\usage{
.sp_cross_dist(a, b)
}
\arguments{
\item{a}{Matrix (n by d).}
\item{b}{Matrix (m by d).}
}
\description{
Internal: pairwise Euclidean distances between two coordinate sets.
}
\value{
Numeric matrix (n by m).
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sp_cross_dist(V, V)
}
