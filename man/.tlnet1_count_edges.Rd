% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlnet1_count_edges}
\alias{.tlnet1_count_edges}
\title{Private helper: count total directed edges (i -> j for j in F_i)}
\usage{
.tlnet1_count_edges(friends)
}
\arguments{
\item{friends}{A vector; its length is taken.}
}
\description{
A step of the tlnet1_native implementation. Called by \code{network_influence_variance}. See the file header for the source the module follows.
}
\value{
The value of \code{total}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tlnet1_count_edges(V)
}
