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
\item{Y}{Passed to \code{.funmix_rows}.}
\item{K}{A count; the body uses it as \code{seq_len(...)}.}
\item{t}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_basis}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{degree}{Passed to \code{.funmix_knots}. Defaults to \code{3L}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{300L}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
\item{var_floor}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
}
\description{
A step of the funmix_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{labels}, \code{posterior}, \code{proportions}, \code{coefficients}, \code{variances}, \code{mean_curves}, \code{basis}, \code{knots}, \code{curve_coefficients}, \code{grid}, \code{loglik}, \code{loglik_path}, \code{bic}, \code{aic}, \code{entropy}, \code{n_parameters}, \code{iterations}, \code{converged}, \code{K}, \code{n}, \code{n_basis}, \code{degree}, \code{method}, \code{note}.
}
\keyword{internal}
