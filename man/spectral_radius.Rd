% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{spectral_radius}
\alias{spectral_radius}
\title{spectral_radius}
\usage{
spectral_radius(spec, theta, tol = 1e-06)
}
\arguments{
\item{spec}{Passed to \code{state_matrices}.}
\item{theta}{Passed to \code{state_matrices}.}
\item{tol}{Passed to \code{>=}. Defaults to \code{1e-06}.}
}
\description{
A step of the bats_native implementation. Called by \code{.fit_spec}, \code{is_forecastable}, \code{morie_bats}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
spec <- BatsSpec()
theta <- rep(0.1, n_free(spec))
spectral_radius(spec, theta)
}
