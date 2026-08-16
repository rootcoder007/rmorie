% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_coarsen}
\alias{.grclus_coarsen}
\title{Mate: 0-based, mate[u] = v means u matched with v}
\usage{
.grclus_coarsen(adj, vw, mate)
}
\arguments{
\item{adj}{A vector; its length is taken and its elements indexed.}
\item{vw}{A vector; indexed elementwise.}
\item{mate}{A vector; indexed elementwise.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_bisect}. See the file header for the source the module follows.
}
\value{
A list with \code{adj}, \code{vw}, \code{mapping}.
}
\keyword{internal}
