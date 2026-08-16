% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_gev_quantile}
\alias{morie_evt_gev_quantile}
\title{GEV quantile (Coles 2001 eq. 3.4, non-exceedance p)}
\usage{
morie_evt_gev_quantile(p, mu, sigma, xi)
}
\arguments{
\item{p}{probability in (0, 1)}
}
\description{
GEV quantile (Coles 2001 eq. 3.4, non-exceedance p)
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_evt_gev_quantile(0.9, 5, 1.5, 0.1)
}
