% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_w4b_dijkstra}
\alias{.morie_gr_w4b_dijkstra}
\title{.morie_gr_w4b_dijkstra}
\usage{
.morie_gr_w4b_dijkstra(adj, source, m)
}
\arguments{
\item{adj}{A vector; indexed elementwise.}
\item{source}{See Usage.}
\item{m}{A count; the body uses it as \code{rep(...)}.}
}
\description{
A step of the geron_w4b_native implementation. Called by \code{morie_geron_isomap}. See the file header for the source the module follows.
}
\value{
The value of \code{dist}, as built in the body.
}
\keyword{internal}
\examples{
.morie_gr_w4b_dijkstra(adj = c(1, 2, 3, 4, 5, 6, 7, 8), source = c("a", "b", "c"), m = 5L)
}
