% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gp_posterior_gradient}
\alias{gp_posterior_gradient}
\title{gp_posterior_gradient}
\usage{
gp_posterior_gradient(X, y, xs, kernel = "matern52", amplitude = 1, length_scale = 1, noise = 1e-08, mean = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{See Usage.}
\item{xs}{See Usage.}
\item{kernel}{Passed to \code{.kernel}. Defaults to \code{"matern52"}.}
\item{amplitude}{Passed to \code{.dkernel_dr2}. Defaults to \code{1}.}
\item{length_scale}{Passed to \code{.lengths}. Defaults to \code{1}.}
\item{noise}{Defaults to \code{1e-08}.}
\item{mean}{Defaults to \code{NULL}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{maximise_acquisition}. See the file header for the source the module follows.
}
\value{
A list with \code{grad_mu}, \code{grad_sd}, \code{mu}, \code{sd}.
}
\keyword{internal}
\examples{
gp_posterior_gradient(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), xs = 5L)
}
