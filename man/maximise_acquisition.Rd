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
\item{X}{Passed to \code{gp_posterior}.}
\item{y}{Passed to \code{gp_posterior}.}
\item{best}{Passed to \code{acquire}.}
\item{box}{A vector; its length is taken and its elements indexed.}
\item{acq}{Passed to \code{acquire}. Defaults to \code{"ei"}.}
\item{kernel}{Passed to \code{gp_posterior}. Defaults to \code{"matern52"}.}
\item{amplitude}{Passed to \code{gp_posterior}. Defaults to \code{1}.}
\item{length_scale}{Passed to \code{gp_posterior}. Defaults to \code{1}.}
\item{noise}{Passed to \code{gp_posterior}. Defaults to \code{1e-08}.}
\item{kappa}{Passed to \code{acquire}. Defaults to \code{2}.}
\item{xi}{Passed to \code{acquire}. Defaults to \code{0}.}
\item{starts}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{n_starts}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-08}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{bayopt}. See the file header for the source the module follows.
}
\value{
A list with \code{x}, \code{acq}, \code{n_starts}, \code{evaluations}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(runif(20), 10, 2)
y <- sin(3 * X[, 1]) + X[, 2]
r <- maximise_acquisition(X, y, best = max(y),
                          box = list(c(0, 1), c(0, 1)),
                          acq = "ei", n_starts = 3, max_iter = 20)
str(r, max.level = 1)
}
