% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_parents}
\alias{.morie_ml_parents}
\title{.morie_ml_parents}
\usage{
.morie_ml_parents(edges, nodes)
}
\arguments{
\item{edges}{A matrix; indexed by row and column.}
\item{nodes}{A vector; its length is taken.}
}
\description{
A step of the causal_molak implementation. Called by \code{.morie_ml_dsep}, \code{morie_dseptest}. See the file header for the source the module follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
