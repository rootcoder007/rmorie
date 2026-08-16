% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_subgraph}
\alias{.grclus_subgraph}
\title{Members: 0-based}
\usage{
.grclus_subgraph(adj, vw, members)
}
\arguments{
\item{adj}{A vector; indexed elementwise.}
\item{vw}{A vector; indexed elementwise.}
\item{members}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the grclus_native implementation. Called by \code{morie_grclus}. See the file header for the source the module follows.
}
\value{
A list with \code{adj}, \code{vw}.
}
\keyword{internal}
