% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{walk}
\alias{walk}
\title{walk}
\usage{
walk(adj, start, length, p = 1, q = 1, seed = 0, weights = NULL)
}
\arguments{
\item{adj}{See Usage.}
\item{start}{See Usage.}
\item{length}{See Usage.}
\item{p}{Defaults to \code{1}.}
\item{q}{Defaults to \code{1}.}
\item{seed}{Defaults to \code{0}.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
A step of the node2v_native implementation. Called by \code{.avalon_paths}, \code{.depth_counts}, \code{.dmlqs_count_totters} and 13 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{path}, as built in the body.
}
\keyword{internal}
