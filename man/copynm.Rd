% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{copynm}
\alias{copynm}
\title{Circular binary segmentation (Olshen et al. 2004)}
\usage{
copynm(x, alpha = 0.01, permutations = 1000L, min_width = 2L, undo_splits = TRUE, seed = 0L, max_depth = 50L)
}
\arguments{
\item{x}{numeric vector of log-ratio intensities along a chromosome.}
\item{alpha}{type I error for testing a single segment. The paper        warns the recursion is not corrected for multiple testing.}
\item{permutations}{number of permutations P. Early stopping makes        large P cheap when the null is clearly true.}
\item{min_width}{minimum segment length to attempt a split on.}
\item{undo_splits}{apply the edge-effect undo.}
\item{seed}{integer seed for the permutation stream.}
\item{max_depth}{recursion guard.}
}
\description{
Segment a copy-number profile by circular binary segmentation. Recursively finds the (i, j) maximising Z_C and tests against a permutation reference with early stopping. Ternary splits are undone if either change-point is not viable as a binary split.
}
\value{
list with `segments` (list of (start, end, mean) half-open),         `changepoints`, `n_segments`, `fitted`, `pvalues`,         `estimate` (alias of segments), `alpha`, `n`,         `multiplicity_note`, `method`.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
copynm(V)
}
