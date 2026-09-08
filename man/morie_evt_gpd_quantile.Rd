% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_gpd_quantile}
\alias{morie_evt_gpd_quantile}
\title{GPD quantile (inverse of Coles 2001 eq. 4.2)}
\usage{
morie_evt_gpd_quantile(p, sigma, xi) 
}
\arguments{
\item{p}{probability in [0, 1)}

\item{sigma}{Argument \code{sigma}; see Usage.}

\item{xi}{Argument \code{xi}; see Usage.}
}
\description{
GPD quantile (inverse of Coles 2001 eq. 4.2)
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_evt_gpd_quantile(p = 0.5, sigma = 0.5, xi = 5L)
}
