% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SignlessL}
\alias{SignlessL}
\title{\\' Signless Laplacian Q = D + A of an undirected graph}
\usage{
SignlessL(A)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
}
\description{
\\' \\' D is the diagonal matrix of row sums.  The multiplicity of 0 as an \\' eigenvalue of Q equals the number of bipartite components; that count \\' is returned, computed combinatorially by two-colouring rather than by \\' an eigensolver, so it is exact. \\' \\' @param A symmetric adjacency matrix with a zero diagonal \\' @return list(Q, degree, n, m, trace, n_components, \\' bipartite_components, zero_eigenvalue_multiplicity, method) \\' @export
}
\value{
A list with \code{Q}, \code{degree}, \code{n}, \code{m}, \code{trace}, \code{n_components}, \code{bipartite_components}, \code{zero_eigenvalue_multiplicity}, \code{method}.
}
\keyword{internal}
