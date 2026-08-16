% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funmix_functional_mixture}
\alias{morie_funmix_functional_mixture}
\title{morie_funmix_functional_mixture}
\usage{
morie_funmix_functional_mixture(Y, K, t = NULL, n_basis = 5L, degree = 3L, max_iter = 300L, tol = 1e-10, var_floor = 1e-08)
}
\arguments{
\item{Y}{See Usage.}
\item{K}{See Usage.}
\item{t}{Defaults to \code{NULL}.}
\item{n_basis}{Defaults to \code{5L}.}
\item{degree}{Defaults to \code{3L}.}
\item{max_iter}{Defaults to \code{300L}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{var_floor}{Defaults to \code{1e-08}.}
}
\description{
Part of the funmix_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{labels}, \code{posterior}, \code{proportions}, \code{coefficients}, \code{variances}, \code{mean_curves}, \code{basis}, \code{knots}, \code{curve_coefficients}, \code{grid}, \code{loglik}, \code{loglik_path}, \code{bic}, \code{aic}, \code{entropy}, \code{n_parameters}, \code{iterations}, \code{converged}, \code{K}, \code{n}, \code{n_basis}, \code{degree}, \code{method}, \code{note}.
}
\keyword{internal}
