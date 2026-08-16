% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{adapted_pagerank}
\alias{adapted_pagerank}
\title{adapted_pagerank}
\usage{
adapted_pagerank(adjacency, nodes, p = NULL, d = 0.7, iters = 200, tol = 1e-12)
}
\arguments{
\item{adjacency}{See Usage.}
\item{nodes}{See Usage.}
\item{p}{Defaults to \code{NULL}.}
\item{d}{Defaults to \code{0.7}.}
\item{iters}{Defaults to \code{200}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
Part of the tagRC_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{w}, \code{ranking}.
}
\keyword{internal}
