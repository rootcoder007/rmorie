% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{centrality_encoding}
\alias{centrality_encoding}
\title{Learnable vector per degree, added at the input layer}
\usage{
centrality_encoding(adj, n, z_in, z_out = NULL, directed = FALSE) 
}
\arguments{
\item{adj}{Adjacency list.}
\item{n}{Number of vertices.}
\item{z_in}{List of degree-indexed learnable vectors (in-degree).}
\item{z_out}{Optional list of out-degree vectors.}
\item{directed}{If TRUE, combine in- and out-degree vectors.}
}
\description{
Learnable vector per degree, added at the input layer
}
\value{
List with encoding, degrees, note.
}
\keyword{internal}
\examples{
centrality_encoding(adj = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), n = 5L, z_in = c(1, 2, 3, 4, 5, 6, 7, 8))
}
