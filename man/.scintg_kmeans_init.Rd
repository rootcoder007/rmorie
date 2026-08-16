% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scintg_kmeans_init}
\alias{.scintg_kmeans_init}
\title{.scintg_kmeans_init}
\usage{
.scintg_kmeans_init(Zn, K, seed)
}
\arguments{
\item{Zn}{A matrix; indexed by row and column.}
\item{K}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the scintg_native implementation. Called by \code{.scintg_maximum_diversity_clustering}. See the file header for the source the module follows.
}
\value{
The value of \code{centres}, as built in the body.
}
\keyword{internal}
