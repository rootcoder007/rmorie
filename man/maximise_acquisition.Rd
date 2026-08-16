% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{maximise_acquisition}
\alias{maximise_acquisition}
\title{maximise_acquisition}
\usage{
maximise_acquisition(X, y, best, box, acq = "ei", kernel = "matern52", amplitude = 1, length_scale = 1, noise = 1e-08, kappa = 2, xi = 0, starts = NULL, n_starts = 8, max_iter = 60, tol = 1e-08, seed = 0)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{best}{See Usage.}
\item{box}{A vector; its length is taken and its elements indexed.}
\item{acq}{Defaults to \code{"ei"}.}
\item{kernel}{Defaults to \code{"matern52"}.}
\item{amplitude}{Defaults to \code{1}.}
\item{length_scale}{Defaults to \code{1}.}
\item{noise}{Defaults to \code{1e-08}.}
\item{kappa}{Defaults to \code{2}.}
\item{xi}{Defaults to \code{0}.}
\item{starts}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{n_starts}{Defaults to \code{8}.}
\item{max_iter}{Defaults to \code{60}.}
\item{tol}{Defaults to \code{1e-08}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{bayopt}. See the file header for the source the module follows.
}
\value{
A list with \code{x}, \code{acq}, \code{n_starts}, \code{evaluations}.
}
\keyword{internal}
