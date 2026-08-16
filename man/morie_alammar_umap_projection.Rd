% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_umap_projection}
\alias{morie_alammar_umap_projection}
\title{UMAP objective minimised by descent (McInnes et al. 2018,}
\usage{
morie_alammar_umap_projection(X, n_neighbors = 5, min_dist = 0.1, d_out = 2, n_steps = 200, learning_rate = 0.05, seed = 1)
}
\arguments{
\item{X}{Points.}
\item{n_neighbors,min_dist,d_out,n_steps,learning_rate,seed}{Settings.}
}
\description{
simplified: exact k-NN, bisected sigma, full-batch descent)
}
\value{
A list with \code{embedding}, \code{objective_initial}, \code{objective_final}, \code{objective_decreased}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_umap_projection(V)
}
