% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_edges}
\alias{.morie_ml_edges}
\title{A DAG is a two-column character matrix / data frame of edges, or a}
\usage{
.morie_ml_edges(dag)
}
\arguments{
\item{dag}{A matrix; passed to \code{as.matrix}.}
}
\description{
list(node = c(children)). Both spellings normalize to a sorted character matrix of edges.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_ml_edges(V)
}
