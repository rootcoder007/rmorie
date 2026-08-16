% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.unitigs}
\alias{.unitigs}
\title{Maximal non-branching paths: the part of the assembly that is}
\usage{
.unitigs(edges, indeg, outdeg)
}
\arguments{
\item{edges}{A vector; indexed elementwise.}
\item{indeg}{A vector; indexed elementwise.}
\item{outdeg}{A vector; indexed elementwise.}
}
\description{
unambiguous even when the whole graph is not. Walks each edge out of every non-simple vertex (in sorted order) and through every simple vertex until a branch point; a graph that is one pure cycle contributes the cycle itself.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
