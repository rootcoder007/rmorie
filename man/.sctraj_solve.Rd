% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sctraj_solve}
\alias{.sctraj_solve}
\title{Solve A x = b; singular pooled covariance gets the paper\\'s advice}
\usage{
.sctraj_solve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{b}{See Usage.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj_cluster_distances}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
