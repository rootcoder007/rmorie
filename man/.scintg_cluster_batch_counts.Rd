% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scintg_cluster_batch_counts}
\alias{.scintg_cluster_batch_counts}
\title{.scintg_cluster_batch_counts}
\usage{
.scintg_cluster_batch_counts(R, batches, names = NULL)
}
\arguments{
\item{R}{A matrix; indexed by row and column.}
\item{batches}{A vector; its length is taken.}
\item{names}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
A step of the scintg_native implementation. Called by \code{.scintg_harmony_objective}, \code{.scintg_maximum_diversity_clustering}. See the file header for the source the module follows.
}
\value{
A list with \code{O}, \code{E}, \code{batches}.
}
\keyword{internal}
