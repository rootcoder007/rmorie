% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_transport}
\alias{.morie_km2_transport}
\title{Exact min-cost transport (successive shortest augmenting path with}
\usage{
.morie_km2_transport(a, b, C)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{A vector; its length is taken.}
\item{C}{A matrix; indexed by row and column.}
}
\description{
Bellman-Ford potentials); ports km122\\'s _solve_transport, and km/kmmsc\\'s WMD reads its objective off the same core.
}
\value{
A list with \code{flow}, \code{u}, \code{v}.
}
\keyword{internal}
