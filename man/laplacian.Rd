% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{laplacian}
\alias{laplacian}
\title{Build the graph Laplacian}
\usage{
laplacian(adj, n, normalized = TRUE) 
}
\arguments{
\item{adj}{Adjacency list.}
\item{n}{Number of vertices.}
\item{normalized}{If TRUE, return I - D^\{-1/2\} A D^\{-1/2\}.}
}
\description{
Build the graph Laplacian
}
\value{
Square Laplacian matrix.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
laplacian(V, V)
}
