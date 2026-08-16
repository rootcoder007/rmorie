% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.setT_rff}
\alias{.setT_rff}
\title{Row-wise feed-forward: rFF(M) = relu(M W1 + b1) W2 + b2}
\usage{
.setT_rff(M, W1, b1, W2, b2)
}
\description{
Row-wise feed-forward: rFF(M) = relu(M W1 + b1) W2 + b2
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.setT_rff(M = c(1, 2, 3, 4, 5, 6, 7, 8), W1 = c(1, 2, 3, 4, 5, 6, 7, 8), b1 = 5L, W2 = c(1, 2, 3, 4, 5, 6, 7, 8), b2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
