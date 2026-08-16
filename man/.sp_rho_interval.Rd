% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_rho_interval}
\alias{.sp_rho_interval}
\title{Internal: `.sp_rho_bounds` shrunk to a closed interval for optimisers.}
\usage{
.sp_rho_interval(W, form = "identity", pad = 1e-06)
}
\arguments{
\item{W}{Weights matrix.}
\item{form}{"identity" or "weighted".}
\item{pad}{Fraction of the width to shrink by at each end.}
}
\description{
Internal: `.sp_rho_bounds` shrunk to a closed interval for optimisers.
}
\value{
Numeric length-2 vector.
}
\keyword{internal}
\examples{
.sp_rho_interval(W = 5L)
}
