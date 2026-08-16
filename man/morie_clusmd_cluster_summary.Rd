% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_clusmd_cluster_summary}
\alias{morie_clusmd_cluster_summary}
\title{Summary of a Butina clustering}
\usage{
morie_clusmd_cluster_summary(clusters)
}
\arguments{
\item{clusters}{Output of \code{morie_clusmd_butina_clusters}.}
}
\description{
Reduce a cluster list (as returned by \code{morie_clusmd_butina_clusters}) to the headline numbers and a flat \code{assignment} vector of length \code{n_compounds} giving each compound\'s 0-indexed cluster index.
}
\value{
A named list with \code{n_clusters}, \code{n_compounds},   \code{sizes}, \code{n_singletons}, \code{assignment},   \code{centroids}.
}
\references{
----------
Butina, D. (1999) "Unsupervised data base clustering based on
Daylight\'s fingerprint and Tanimoto similarity: a fast and automated
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
