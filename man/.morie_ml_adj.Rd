% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_adj}
\alias{.morie_ml_adj}
\title{.morie_ml_adj}
\usage{
.morie_ml_adj(edges, a, b)
}
\arguments{
\item{edges}{A matrix; indexed by row and column.}
\item{a}{Passed to \code{==}.}
\item{b}{Passed to \code{==}.}
}
\description{
A step of the causal_molak implementation. Called by \code{.morie_ml_colliders}, \code{morie_bowarc}, \code{morie_collider} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
.morie_ml_adj(edges = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
}
