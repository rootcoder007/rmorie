% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_gridint}
\alias{.morie_rg_gridint}
\title{Composite Simpson on a uniform grid with an even panel count (error}
\usage{
.morie_rg_gridint(y, x = NULL)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{x}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
O(h^4)); trapezoid otherwise, which is what a non-uniform grid admits without interpolating.
}
\value{
The value of \code{.morie_fsum}.
}
\keyword{internal}
