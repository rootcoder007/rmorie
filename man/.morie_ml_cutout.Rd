% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_cutout}
\alias{.morie_ml_cutout}
\title{.morie_ml_cutout}
\usage{
.morie_ml_cutout(edges, sources)
}
\arguments{
\item{edges}{A matrix; indexed by row and column.}
\item{sources}{Passed to \code{\%in\%}.}
}
\description{
A step of the causal_molak implementation. Called by \code{morie_docalc}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_ml_cutout(M, V)
}
