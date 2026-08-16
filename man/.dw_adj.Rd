% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dw_adj}
\alias{.dw_adj}
\title{Neighbour lists from a square 0/1 adjacency matrix}
\usage{
.dw_adj(G)
}
\arguments{
\item{G}{Passed to \code{.s03mat}.}
}
\description{
A step of the deepw implementation. Called by \code{Comemb}, \code{Deepw}. See the file header for the source the module follows.
}
\value{
A list with \code{A}, \code{n}, \code{nb}.
}
\keyword{internal}
