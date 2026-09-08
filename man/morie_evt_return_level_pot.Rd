% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_return_level_pot}
\alias{morie_evt_return_level_pot}
\title{POT m-observation return level (Coles 2001 eq. 4.12-4.14)}
\usage{
morie_evt_return_level_pot(u, sigma, xi, zeta_u, m) 
}
\arguments{
\item{u}{threshold; sigma,xi GPD parameters; zeta_u = P(X > u);   m observations}

\item{sigma}{Argument \code{sigma}; see Usage.}

\item{xi}{Argument \code{xi}; see Usage.}

\item{zeta_u}{Argument \code{zeta_u}; see Usage.}

\item{m}{Argument \code{m}; see Usage.}
}
\description{
POT m-observation return level (Coles 2001 eq. 4.12-4.14)
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_evt_return_level_pot(u = c(1, 2, 3, 4, 5, 6, 7, 8), sigma = 0.5, xi = 5L,
  zeta_u = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L)
}
