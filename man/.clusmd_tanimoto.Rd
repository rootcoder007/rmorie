% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clusmd_tanimoto}
\alias{.clusmd_tanimoto}
\title{Tanimoto similarity of two fingerprint vectors (sorted int vectors)}
\usage{
.clusmd_tanimoto(a, b)
}
\arguments{
\item{a}{Passed to \code{.clusmd_fp}.}
\item{b}{Passed to \code{.clusmd_fp}.}
}
\description{
c / (a + b - c), guarding the all-zero case like sasimi._guard.
}
\value{
A numeric value.
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
.clusmd_tanimoto(V, V)
}
