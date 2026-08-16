% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_children}
\alias{.morie_ml_children}
\title{.morie_ml_children}
\usage{
.morie_ml_children(edges, nodes)
}
\arguments{
\item{edges}{A matrix; indexed by row and column.}
\item{nodes}{A vector; its length is taken.}
}
\description{
A step of the causal_molak implementation. Called by \code{.morie_ml_acyclic}, \code{.morie_ml_dsep}, \code{morie_docalc} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_ml_children(M, V)
}
