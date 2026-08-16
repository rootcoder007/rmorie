% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.motiff_triads}
\alias{.motiff_triads}
\title{.motiff_triads}
\usage{
.motiff_triads(adj, n)
}
\arguments{
\item{adj}{A matrix; indexed by row and column.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the motiff_native implementation. Called by \code{morie_motiff}. See the file header for the source the module follows.
}
\value{
A list with \code{ffl}, \code{cycle3}.
}
\keyword{internal}
