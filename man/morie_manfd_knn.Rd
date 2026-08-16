% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_manfd_knn}
\alias{morie_manfd_knn}
\title{The k-nearest-neighbour graph, as a weighted adjacency matrix}
\usage{
morie_manfd_knn(D, k, symmetric = TRUE)
}
\arguments{
\item{D}{A distance matrix.}
\item{k}{Neighbours per point.}
\item{symmetric}{Whether to symmetrise by union.}
}
\description{
Symmetrised by union: i and j are joined if EITHER lists the other. Without that the graph is directed and the shortest path between two points can depend on which way you walk it, which a distance may not do.
}
\value{
The adjacency matrix, infinite where there is no edge.
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
N <- 18L
P <- 12L
GRID <- vapply(0:(P - 1L), function(t) t/(P - 1), numeric(1))
Y <- matrix(0, N, P)
D <- morie_manfd_l2(Y, GRID)
morie_manfd_knn(D, 3L)
}
