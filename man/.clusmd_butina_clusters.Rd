% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clusmd_butina_clusters}
\alias{.clusmd_butina_clusters}
\title{.clusmd_butina_clusters}
\usage{
.clusmd_butina_clusters(fps, threshold = 0.8, recount = FALSE)
}
\arguments{
\item{fps}{See Usage.}
\item{threshold}{Defaults to \code{0.8}.}
\item{recount}{Defaults to \code{FALSE}.}
}
\description{
Butina clusters: the exclusion-sphere clusters, largest sphere first. --------------------------------------------------------------------------- Mirrors Python butina_clusters(): repeatedly pick the live compound with the largest (live-respecting) neighbour count as a centroid, then take the centroid together with its live neighbours as the next cluster and remove them. Ties broken by ascending index for determinism. Final list is sorted by (-size, centroid).
}
\value{
The value of \code{clusters}, as built in the body.
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
