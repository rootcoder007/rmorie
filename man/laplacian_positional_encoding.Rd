% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{laplacian_positional_encoding}
\alias{laplacian_positional_encoding}
\title{Laplacian eigenvector positional encoding}
\usage{
laplacian_positional_encoding(adj, n, dim = 2L, normalized = TRUE)
}
\arguments{
\item{adj}{Adjacency list.}
\item{n}{Number of vertices.}
\item{dim}{Number of non-trivial eigenvectors.}
\item{normalized}{If TRUE, use the normalised Laplacian.}
}
\description{
Laplacian eigenvector positional encoding
}
\value{
List with encoding, eigenvalues, caveat.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
laplacian_positional_encoding(V, V)
}
