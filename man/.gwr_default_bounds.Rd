% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwr_default_bounds}
\alias{.gwr_default_bounds}
\title{.gwr_default_bounds}
\usage{
.gwr_default_bounds(C)
}
\arguments{
\item{C}{A matrix; indexed by row and column.}
}
\description{
A step of the gwrcal_native implementation. Called by \code{morie_gwrcal}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.gwr_default_bounds(M)
}
