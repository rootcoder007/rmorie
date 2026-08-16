% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clusmd_neighbour_lists}
\alias{.clusmd_neighbour_lists}
\title{.clusmd_neighbour_lists}
\usage{
.clusmd_neighbour_lists(fps, threshold = 0.8)
}
\arguments{
\item{fps}{A vector; its length is taken.}
\item{threshold}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.8}.}
}
\description{
Neighbour lists: for each compound, the set of others within threshold. --------------------------------------------------------------------------- Returns a list of integer vectors (sorted ascending); index i holds the indices j != i with Tanimoto(fp_i, fp_j) >= threshold. Mirrors the Python neighbour_lists() function.
}
\value{
The value of \code{nb}, as built in the body.
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
.clusmd_neighbour_lists(V)
}
