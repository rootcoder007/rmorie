% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{to_approx_dp}
\alias{to_approx_dp}
\title{Proposition 1.3: (rho + 2 sqrt(rho log(1/delta)), delta)-DP}
\usage{
to_approx_dp(rho, delta) 
}
\arguments{
\item{rho}{zCDP parameter, must be positive.}
\item{delta}{Failure probability, must lie in (0, 1).}
}
\description{
Proposition 1.3: (rho + 2 sqrt(rho log(1/delta)), delta)-DP
}
\value{
A list with \code{epsilon}, \code{delta}, \code{rho}.
}
\keyword{internal}
\examples{
to_approx_dp(rho = 0.5, delta = 0.5)
}
