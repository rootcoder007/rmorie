% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_manfd_paths}
\alias{morie_manfd_paths}
\title{All-pairs shortest paths by Floyd-Warshall, and the components}
\usage{
morie_manfd_paths(A) 
}
\arguments{
\item{A}{An adjacency matrix.}
}
\description{
An unreachable pair keeps an infinite distance, which is the truth about it.
}
\value{
A list with the geodesic matrix and the component count.
}
\references{
  Chen, D. and Mueller, H.-G. (2012) "Nonlinear manifold
    representations for functional data." The Annals of Statistics
    40(1), 1-29. doi:10.1214/11-AOS936.
  Tenenbaum, J.B., de Silva, V. and Langford, J.C. (2000) "A global
    geometric framework for nonlinear dimensionality reduction."
    Science 290(5500), 2319-2323.
  Torgerson, W.S. (1952) "Multidimensional scaling: I. Theory and
    method." Psychometrika 17(4), 401-419.
  Floyd, R.W. (1962) "Algorithm 97: shortest path." Communications of
    the ACM 5(6), 345.
  Jacobi, C.G.J. (1846) Journal fuer die reine und angewandte
    Mathematik 30, 51-94.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_manfd_paths(M)
}
