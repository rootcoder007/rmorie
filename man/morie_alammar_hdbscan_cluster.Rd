% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_hdbscan_cluster}
\alias{morie_alammar_hdbscan_cluster}
\title{Density clustering, HDBSCAN style (Campello et al. 2013)}
\usage{
morie_alammar_hdbscan_cluster(X, min_cluster_size = 3, min_samples = NULL)
}
\arguments{
\item{X}{Point matrix.}
\item{min_cluster_size,min_samples}{Sizes.}
}
\description{
Core distance to the min_samples-th neighbour, mutual reachability, single-linkage MST, and a flat cut chosen to MAXIMISE the number of clusters of size >= min_cluster_size (a single largest-gap cut fails on blobs plus one far outlier). Noise is -1. The Prim implementation\'s visited mask is load-bearing -- overwriting the Inf of in-tree nodes re-admits them, the bug the Python mirror caught on planted blobs.
}
\value{
A list with \code{labels}, \code{n_clusters}, \code{n_noise}, \code{core_distances}, \code{cut_threshold}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_hdbscan_cluster(V)
}
