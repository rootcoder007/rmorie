% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{NoiseModel}
\alias{NoiseModel}
\title{Eqs (3.12)-(3.14).  Eq (3.14) holds only if x and eta are}
\usage{
NoiseModel(x, eta) 
}
\arguments{
\item{x}{Passed to \code{.morie_rg_aslist}.}
\item{eta}{Passed to \code{.morie_rg_aslist}.}
}
\description{
uncorrelated, so the sample correlation is measured and both the observed and the additive variance are returned side by side.
}
\value{
A list with \code{y}, \code{mean_signal}, \code{mean_noise}, \code{mean_observed}, \code{mean_additive}, \code{variance_observed}, \code{variance_additive}, \code{covariance}, \code{correlation}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
NoiseModel(V, V)
}
