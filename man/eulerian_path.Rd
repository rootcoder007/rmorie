% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{eulerian_path}
\alias{eulerian_path}
\title{Hierholzer\'s algorithm with the exact existence condition checked:}
\usage{
eulerian_path(edges, indeg, outdeg) 
}
\arguments{
\item{edges}{A vector; indexed elementwise.}
\item{indeg}{A vector; indexed elementwise.}
\item{outdeg}{A vector; indexed elementwise.}
}
\description{
connected, and either all in/out degrees equal, or exactly one vertex with out - in = 1 (start) and one with in - out = 1 (end). Returns NULL when no Eulerian path exists -- ambiguous or disconnected graphs are reported, not guessed at.
}
\value{
The value of \code{path}, as built in the body.
}
\keyword{internal}
