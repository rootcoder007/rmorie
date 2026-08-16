% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gp_posterior}
\alias{gp_posterior}
\title{gp_posterior}
\usage{
gp_posterior(X, y, Xs, kernel = "matern52", amplitude = 1, length_scale = 1, noise = 1e-08, mean = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{Xs}{See Usage.}
\item{kernel}{Defaults to \code{"matern52"}.}
\item{amplitude}{Defaults to \code{1}.}
\item{length_scale}{Defaults to \code{1}.}
\item{noise}{Defaults to \code{1e-08}.}
\item{mean}{Defaults to \code{NULL}.}
}
\description{
Part of the bayopt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{mean}, \code{variance}, \code{sd}.
}
\keyword{internal}
\examples{
gp_posterior(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), Xs = c(1, 2, 3, 4, 5, 6, 7, 8))
}
