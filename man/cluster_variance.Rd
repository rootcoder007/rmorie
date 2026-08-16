% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cluster_variance}
\alias{cluster_variance}
\title{cluster_variance}
\usage{
cluster_variance(ic, cluster)
}
\arguments{
\item{ic}{See Usage.}
\item{cluster}{See Usage.}
}
\description{
A step of the tlclust_native implementation. Called by \code{design_effect}, \code{morie_tlclust}. See the file header for the source the module follows.
}
\value{
A list with \code{se}, \code{n_clusters}, \code{cluster_sums}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cluster_variance(V, V)
}
