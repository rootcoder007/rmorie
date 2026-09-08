% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gp_posterior}
\alias{gp_posterior}
\title{gp_posterior}
\usage{
gp_posterior(X, y, Xs, kernel = "matern52", amplitude = 1, length_scale = 1, 
    noise = 1e-08, mean = NULL) 
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Xs}{A matrix; passed to \code{as.matrix}.}
\item{kernel}{Passed to \code{.kernel}. Defaults to \code{"matern52"}.}
\item{amplitude}{Passed to \code{k}. Defaults to \code{1}.}
\item{length_scale}{Passed to \code{k}. Defaults to \code{1}.}
\item{noise}{The body requires: bayopt: noise must be non-negative. Defaults to \code{1e-08}.}
\item{mean}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{bayopt}, \code{maximise_acquisition}. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{variance}, \code{sd}.
}
\keyword{internal}
\examples{
gp_posterior(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8),
  Xs = c(1, 2, 3, 4, 5, 6, 7, 8))
}
