% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clusmd_fp}
\alias{.clusmd_fp}
\title{.clusmd_fp}
\usage{
.clusmd_fp(bits, n_bits = NULL)
}
\arguments{
\item{bits}{A matrix; indexed by row and column.}
\item{n_bits}{Defaults to \code{NULL}.}
}
\description{
Fingerprint normalisation -- mirror sasimi.fingerprint on the R side. --------------------------------------------------------------------------- Accepts an integer set, a numeric vector of indices, or a 0/1 sequence. Returns an integer vector of on-bit indices (sorted, deduped).
}
\value{
A vector, from \code{sort}.
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
