% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{edge_encoding}
\alias{edge_encoding}
\title{Average edge features along the shortest path}
\usage{
edge_encoding(paths, edge_features, w_table) 
}
\arguments{
\item{paths}{List of (i, j) -> ordered edge tuples along the path.}
\item{edge_features}{Edge feature lookup (e or its reverse).}
\item{w_table}{Learnable weight table indexed by step.}
}
\description{
Average edge features along the shortest path
}
\value{
List with edge_bias and note.
}
\keyword{internal}
\examples{
edge_encoding(paths = c(1, 2, 3, 4, 5, 6, 7, 8), edge_features = c(1, 2, 3, 4, 5, 6, 7, 8), w_table = c(1, 2, 3, 4, 5, 6, 7, 8))
}
