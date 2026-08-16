% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gbTagged}
\alias{.gbTagged}
\title{Pooled indicator vector: 1 where the i-th smallest is an X}
\usage{
.gbTagged(xs, ys)
}
\description{
Pooled indicator vector: 1 where the i-th smallest is an X
}
\value{
A vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gbTagged(V, V)
}
