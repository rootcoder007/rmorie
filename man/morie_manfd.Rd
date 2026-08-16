% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_manfd}
\alias{morie_manfd}
\title{Embed functional data on its manifold}
\usage{
morie_manfd(Y, k = 4L, method = "isomap", grid = NULL, dim = 2L, sweeps = 60L)
}
\arguments{
\item{Y}{A matrix with one curve per row.}
\item{k}{Neighbours per point in the graph.}
\item{method}{A member of the method list.}
\item{grid}{The sampling points, or NULL.}
\item{dim}{Embedding dimension.}
\item{sweeps}{Jacobi sweeps.}
}
\description{
Embed functional data on its manifold
}
\value{
A list with the coordinates, the eigenvalues, the geodesic   distances, the number of connected components and the residual   variance of the embedding.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_manfd(V)
}
