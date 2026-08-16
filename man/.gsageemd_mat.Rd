% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gsageemd_mat}
\alias{.gsageemd_mat}
\title{Aggregate a set of neighbour vectors}
\usage{
.gsageemd_mat(x)
}
\arguments{
\item{vectors}{Numeric matrix (one row per neighbour) or list of   rows.}
\item{how}{One of "mean", "max_pool", "lstm_order".}
\item{W}{Optional weight matrix for max_pool.}
}
\description{
Aggregate a set of neighbour vectors
}
\value{
Permutation-invariant summary vector.
}
\keyword{internal}
