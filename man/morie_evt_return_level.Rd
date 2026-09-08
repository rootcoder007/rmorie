% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_return_level}
\alias{morie_evt_return_level}
\title{GEV T-period return level (Coles 2001 eq. 3.4/3.10)}
\usage{
morie_evt_return_level(mu, sigma, xi, T) 
}
\arguments{
\item{T}{return period (> 1)}

\item{mu}{Argument \code{mu}; see Usage.}

\item{sigma}{Argument \code{sigma}; see Usage.}

\item{xi}{Argument \code{xi}; see Usage.}
}
\description{
GEV T-period return level (Coles 2001 eq. 3.4/3.10)
}
\value{
The value of \code{morie_evt_gev_quantile}.
}
\keyword{internal}
\examples{
morie_evt_return_level(5, 1.5, 0.1, 100)
}
