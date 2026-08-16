% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_clusmd_neighbour_lists}
\alias{morie_clusmd_neighbour_lists}
\title{Neighbour lists within a Tanimoto threshold}
\usage{
morie_clusmd_neighbour_lists(fps, threshold = 0.8)
}
\arguments{
\item{fps}{A list of fingerprints. Each fingerprint is either an   integer vector of on-bit indices or a 0/1 sequence.}
\item{threshold}{Tanimoto threshold in \code{[0, 1]}; default 0.8   (Butina\'s working value).}
}
\description{
For each compound, return the integer vector of indices of the other compounds whose Tanimoto similarity to it is at least \code{threshold}. Mirrors \code{morie.fn.clusmd.neighbour_lists}.
}
\value{
A list of length \code{length(fps)} of integer vectors.
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
morie_clusmd_neighbour_lists(V)
}
