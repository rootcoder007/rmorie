% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dmlqs_norm_adj}
\alias{.dmlqs_norm_adj}
\title{Coerce an adjacency mapping to a list keyed by character atom id,}
\usage{
.dmlqs_norm_adj(adj)
}
\arguments{
\item{adj}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
}
\description{
each value a sorted unique integer vector of neighbours (with v itself excluded -- mirrors Python `set(adj[v]) - {v}`).
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.dmlqs_norm_adj(M)
}
