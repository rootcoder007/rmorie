% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dimNet_triplet_count}
\alias{morie_dimNet_triplet_count}
\title{Triplet count of a directed graph}
\usage{
morie_dimNet_triplet_count(adj)
}
\arguments{
\item{adj}{Named list mapping each node to its neighbours.}
}
\description{
Triplet count of a directed graph
}
\value{
A list with \code{triplets} and \code{pairs}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_dimNet_triplet_count(V)
}
