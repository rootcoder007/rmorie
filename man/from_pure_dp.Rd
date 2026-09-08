% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{from_pure_dp}
\alias{from_pure_dp}
\title{Proposition 1.4: epsilon-DP is 0.5 epsilon^2-zCDP}
\usage{
from_pure_dp(epsilon) 
}
\arguments{
\item{epsilon}{Pure DP parameter, must be non-negative.}
}
\description{
Proposition 1.4: epsilon-DP is 0.5 epsilon^2-zCDP
}
\value{
A list with \code{rho} and \code{epsilon}.
}
\keyword{internal}
\examples{
from_pure_dp(epsilon = 5L)
}
