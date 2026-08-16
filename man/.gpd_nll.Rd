% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpd_nll}
\alias{.gpd_nll}
\title{Negative log-likelihood of the GPD at (sigma, xi) for excesses y}
\usage{
.gpd_nll(y, sigma, xi)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{sigma}{Numeric; passed to \code{log}.}
\item{xi}{Numeric; passed to \code{abs}.}
}
\description{
A step of the evpot implementation. Called by \code{Evpot}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.gpd_nll(y = c(1, 2, 3, 4, 5, 6, 7, 8), sigma = 0.5, xi = 5L)
}
