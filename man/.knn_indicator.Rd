% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.knn_indicator}
\alias{.knn_indicator}
\title{Internal: a_ij = 1 when j is among i\\'s k nearest, excluding i itself}
\usage{
.knn_indicator(D, k)
}
\arguments{
\item{D}{A matrix; indexed by row and column.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the panelspatial implementation. Called by \code{morie_jacquez_knn}. See the file header for the source the module follows.
}
\value{
The value of \code{A}, as built in the body.
}
\keyword{internal}
