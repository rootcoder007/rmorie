% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{shortest_path_matrix}
\alias{shortest_path_matrix}
\title{All-pairs shortest path lengths by BFS}
\usage{
shortest_path_matrix(adj, n)
}
\arguments{
\item{adj}{Adjacency list.}
\item{n}{Number of vertices.}
}
\description{
All-pairs shortest path lengths by BFS
}
\value{
List with distance matrix, unreachable, n_unreachable.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
shortest_path_matrix(D, V)
}
