% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.connected}
\alias{.connected}
\title{Weak connectivity over vertices that carry at least one edge, used}
\usage{
.connected(edges, verts)
}
\arguments{
\item{edges}{A vector; indexed elementwise.}
\item{verts}{Accepted by the signature and not used anywhere in the body.}
}
\description{
by eulerian_path to enforce the "connected on its non-isolated vertices" half of the Eulerian existence condition.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
.connected(edges = list(a = 1, b = 2), verts = c(1, 2, 3, 4, 5, 6, 7, 8))
}
