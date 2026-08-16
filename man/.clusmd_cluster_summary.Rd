% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clusmd_cluster_summary}
\alias{.clusmd_cluster_summary}
\title{.clusmd_cluster_summary}
\usage{
.clusmd_cluster_summary(clusters)
}
\arguments{
\item{clusters}{See Usage.}
}
\description{
Cluster summary: sizes, singleton count, and the assignment. --------------------------------------------------------------------------- Mirrors Python cluster_summary(). `assignment[m]` = k means compound m is in cluster k (0-indexed).
}
\value{
A list with \code{n_clusters}, \code{n_compounds}, \code{sizes}, \code{n_singletons}, \code{assignment}, \code{centroids}.
}
\references{
----------
Butina, D. (1999) "Unsupervised data base clustering based on
Daylight\\'s fingerprint and Tanimoto similarity: a fast and automated
way to cluster small and large data sets", *Journal of Chemical
Information and Computer Sciences* 39(4), 747-750,
doi:10.1021/ci9803381. The neighbour-count ordering, the exclusion
sphere around each selected centroid, the singleton handling, and the
0.8 Tanimoto working threshold reproduced as the default.

Willett, P., Barnard, J. M. & Downs, G. M. (1998) "Chemical
similarity searching", *Journal of Chemical Information and Computer
Sciences* 38(6), 983-996, doi:10.1021/ci9800211, for the Tanimoto
coefficient itself.

Native R port of morie.fn.clusmd. No external packages; the Tanimoto
coefficient on binary fingerprints is computed inline (c / (a + b - c))
so this arm is independent of sasimi_native.R.
}
\keyword{internal}
