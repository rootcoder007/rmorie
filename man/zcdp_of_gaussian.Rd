% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{zcdp_of_gaussian}
\alias{zcdp_of_gaussian}
\title{Proposition 1.6: rho = Delta^2 / (2 sigma^2)}
\usage{
zcdp_of_gaussian(sensitivity, sigma)
}
\arguments{
\item{sensitivity}{Sensitivity Delta, must be non-negative.}
\item{sigma}{Standard deviation, must be positive.}
}
\description{
Proposition 1.6: rho = Delta^2 / (2 sigma^2)
}
\value{
A scalar numeric: the zCDP parameter rho.
}
\keyword{internal}
\examples{
zcdp_of_gaussian(sensitivity = 5L, sigma = 0.5)
}
