% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sigma_for_rho}
\alias{sigma_for_rho}
\title{The noise scale that buys a given rho}
\usage{
sigma_for_rho(sensitivity, rho) 
}
\arguments{
\item{sensitivity}{Sensitivity Delta, must be non-negative.}
\item{rho}{zCDP parameter, must be positive.}
}
\description{
The noise scale that buys a given rho
}
\value{
A scalar numeric: the Gaussian standard deviation sigma.
}
\keyword{internal}
\examples{
sigma_for_rho(sensitivity = 5L, rho = 0.5)
}
