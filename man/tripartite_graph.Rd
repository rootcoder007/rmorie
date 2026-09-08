% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tripartite_graph}
\alias{tripartite_graph}
\title{tripartite_graph}
\usage{
tripartite_graph(triples) 
}
\arguments{
\item{triples}{A vector; its length is taken.}
}
\description{
A step of the tagRC_native implementation. Called by \code{folkrank}. See the file header for the source the module follows.
}
\value{
A list with \code{adjacency}, \code{nodes}, \code{n_nodes}, \code{n_triples}, \code{note}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
tripartite_graph(D)
}
