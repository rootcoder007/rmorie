% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj_minimum_spanning_tree}
\alias{morie_sctraj_minimum_spanning_tree}
\title{Prim\'s MST, with the paper\'s optional terminal-state constraint}
\usage{
morie_sctraj_minimum_spanning_tree(D, clusters, ends = NULL)
}
\arguments{
\item{D}{A matrix; indexed by row and column.}
\item{clusters}{See Usage.}
\item{ends}{Defaults to \code{NULL}.}
}
\description{
With ends given, the tree is built on the non-terminal clusters and each terminal is attached to its nearest non-terminal neighbour.
}
\value{
A list with \code{edges}, \code{adjacency}, \code{nodes}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_sctraj_minimum_spanning_tree(D, D)
}
