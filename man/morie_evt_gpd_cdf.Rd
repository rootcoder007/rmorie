% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_gpd_cdf}
\alias{morie_evt_gpd_cdf}
\title{GPD distribution function (Coles 2001 eq. 4.2-4.4)}
\usage{
morie_evt_gpd_cdf(y, sigma, xi)
}
\arguments{
\item{y}{excess(es) over the threshold, y >= 0}
\item{sigma,xi}{GPD scale (> 0) and shape}
}
\description{
GPD distribution function (Coles 2001 eq. 4.2-4.4)
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
morie_evt_gpd_cdf(y = c(1, 2, 3, 4, 5, 6, 7, 8), sigma = 0.5, xi = 5L)
}
