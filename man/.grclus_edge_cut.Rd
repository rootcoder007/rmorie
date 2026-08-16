% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_edge_cut}
\alias{.grclus_edge_cut}
\title{.grclus_edge_cut}
\usage{
.grclus_edge_cut(adj, parts)
}
\arguments{
\item{adj}{A vector; its length is taken and its elements indexed.}
\item{parts}{A vector; indexed elementwise.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_grow_partition}, \code{.grclus_kl}, \code{morie_grclus}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
