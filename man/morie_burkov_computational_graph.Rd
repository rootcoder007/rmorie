% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_computational_graph}
\alias{morie_burkov_computational_graph}
\title{Computational-graph forward and reverse pass (Burkov Ch 1)}
\usage{
morie_burkov_computational_graph(graph, inputs)
}
\arguments{
\item{graph}{Node list.}
\item{inputs}{Named leaf values.}
}
\description{
`graph` is a topologically ordered list of nodes `list(name =, op =, args = c(...))`; `inputs` a named list of leaf values. Gradients of the last node with respect to every leaf come back exactly; the parity tests pin them against Python\'s autodiff and against central finite differences.
}
\value{
A list with \code{output}, \code{estimate}, \code{gradients}, \code{values}, \code{n}, \code{method}.
}
\keyword{internal}
