% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_gpd_loglik}
\alias{morie_evt_gpd_loglik}
\title{GPD log-likelihood over excesses (Coles 2001 eq. 4.10)}
\usage{
morie_evt_gpd_loglik(y, sigma, xi) 
}
\description{
GPD log-likelihood over excesses (Coles 2001 eq. 4.10)
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{sigma}{Argument \code{sigma}; see Usage.}
\item{xi}{Argument \code{xi}; see Usage.}
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_evt_gpd_loglik(y = c(1, 2, 3, 4, 5, 6, 7, 8), sigma = 0.5, xi = 5L)
}
