% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_clusmd_butina_clusters}
\alias{morie_clusmd_butina_clusters}
\title{Butina exclusion-sphere clusters}
\usage{
morie_clusmd_butina_clusters(fps, threshold = 0.8, recount = FALSE) 
}
\arguments{
\item{fps}{A list of fingerprints.}
\item{threshold}{Tanimoto threshold in \code{[0, 1]}; default 0.8.}
\item{recount}{If \code{TRUE}, recompute neighbour counts each   round against the currently live compounds (more expensive, can   give more balanced clusters). Default \code{FALSE} preserves   the original counts.}
}
\description{
Apply the Butina (1999) exclusion-sphere clustering algorithm to a set of fingerprints. At each round the live compound with the largest neighbour count (live-restricted if \code{recount=TRUE}) is taken as a centroid; the centroid together with its live neighbours form a cluster and are removed. Ties are broken by ascending index. Clusters are returned sorted by descending size then ascending centroid.
}
\value{
A list of clusters, each a named list with   \code{centroid}, \code{members}, and \code{size}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_clusmd_butina_clusters(V)
}
