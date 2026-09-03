% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mpfn_is_permutation_invariant}
\alias{morie_mpfn_is_permutation_invariant}
\title{Check permutation invariance of a graph-level prediction}
\usage{
morie_mpfn_is_permutation_invariant(H, adj, edge_features, perm, T = 3L, how = "sum", tol = 1e-09) 
}
\arguments{
\item{H}{Initial states.}
\item{adj}{Adjacency list.}
\item{edge_features}{Edge feature map keyed by "i_j" (integer pair).}
\item{perm}{0-based permutation of node indices.}
\item{T,how}{arguments.}
\item{tol}{Tolerance.}
}
\description{
Check permutation invariance of a graph-level prediction
}
\value{
A list with invariant, max_deviation, readout.
}
\keyword{internal}
\examples{
morie_mpfn_is_permutation_invariant(H = 0.5,
  adj = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)),
  edge_features = c(1, 2, 3, 4, 5, 6, 7, 8), perm = c(1, 2, 3, 4, 5, 6, 7, 8))
}
