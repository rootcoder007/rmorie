% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bayopt}
\alias{bayopt}
\title{bayopt}
\usage{
bayopt(f, bounds, n_iter = 20, n_init = 5, acq = "ei", kernel = "matern52", amplitude = 1, length_scale = 1, noise = 1e-08, kappa = 2, xi = 0, n_candidates = 200, seed = 0, X0 = NULL, y0 = NULL, inner = "gradient", n_starts = 8)
}
\arguments{
\item{f}{See Usage.}
\item{bounds}{See Usage.}
\item{n_iter}{Defaults to \code{20}.}
\item{n_init}{Defaults to \code{5}.}
\item{acq}{One of \code{"ei"}, \code{"lcb"}, \code{"pi"}. Defaults to \code{"ei"}.}
\item{kernel}{Defaults to \code{"matern52"}.}
\item{amplitude}{Defaults to \code{1}.}
\item{length_scale}{Defaults to \code{1}.}
\item{noise}{Defaults to \code{1e-08}.}
\item{kappa}{Defaults to \code{2}.}
\item{xi}{Defaults to \code{0}.}
\item{n_candidates}{Defaults to \code{200}.}
\item{seed}{Defaults to \code{0}.}
\item{X0}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{y0}{Defaults to \code{NULL}.}
\item{inner}{One of \code{"gradient"}, \code{"random"}. Defaults to \code{"gradient"}.}
\item{n_starts}{Defaults to \code{8}.}
}
\description{
A step of the bayopt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{x_best}, \code{y_best}, \code{X}, \code{y}, \code{trace}, \code{acq}, \code{kernel}, \code{inner}, \code{n_eval}, \code{method}, \code{note}.
}
\keyword{internal}
