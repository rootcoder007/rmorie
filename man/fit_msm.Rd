% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_msm}
\alias{fit_msm}
\title{fit_msm}
\usage{
fit_msm(outcome, cumulative, weights = NULL, states = .STATES)
}
\arguments{
\item{outcome}{See Usage.}
\item{cumulative}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
\item{states}{Defaults to \code{.STATES}.}
}
\description{
Part of the bhltmsm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{intercept}, \code{coefficients}, \code{se}, \code{per_episode}, \code{weighted}, \code{effective_n}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
fit_msm(V, V)
}
