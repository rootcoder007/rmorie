% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_match_vertices}
\alias{.grclus_match_vertices}
\title{Coarsening ----}
\usage{
.grclus_match_vertices(adj, scheme, seed)
}
\arguments{
\item{adj}{A vector; its length is taken and its elements indexed.}
\item{scheme}{One of \code{"hem"}, \code{"rm"}.}
\item{seed}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_bisect}. See the file header for the source the module follows.
}
\value{
The value of \code{mate}, as built in the body.
}
\keyword{internal}
