% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_funmix_functional_mixture}
\alias{morie_funmix_functional_mixture}
\alias{morie_funmix}
\title{Functional clustering: a K-component Gaussian mixture}
\usage{
morie_funmix_functional_mixture(Y, K, t = NULL, n_basis = 5L, degree = 3L, max_iter = 300L, tol = 1e-10, var_floor = 1e-08)
}
\description{
R arm of funmix -- functional clustering: a K-component Gaussian mixture 2nd ed., Ch. 3.
}
\value{
A list with components \code{n}, \code{nrow}, \code{lab0}, \code{drop}, \code{estimate}, \code{labels}, \code{posterior}, \code{proportions}, \code{coefficients}, \code{variances}, \code{mean_curves}, \code{basis}, \code{knots}, \code{curve_coefficients}, \code{grid}, \code{loglik}, \code{loglik_path}, \code{bic}, \code{aic}, \code{entropy}, \code{n_parameters}, \code{iterations}, \code{converged}, \code{K}, \code{n_basis}, \code{degree}, \code{method}, \code{note}.
}
\references{
on B-spline coefficients fitted by EM. James, G. M. & Sugar, C. A. (2003)
JASA 98(462), 397-408; de Boor, C. (1978) A Practical Guide to Splines,
Ch. IX; Ramsay, J. O. & Silverman, B. W. (2005) Functional Data Analysis,
}
\keyword{internal}
