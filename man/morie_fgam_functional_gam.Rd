% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_fgam_functional_gam}
\alias{morie_fgam_functional_gam}
\alias{morie_fgam}
\title{The functional generalized additive model}
\usage{
morie_fgam_functional_gam(X, Y, basis = NULL, n_x = 6, n_t = 6, lam_x = 1, lam_t = 1)
}
\description{
R arm of fgam -- the functional generalized additive model   E[Y|X] = theta0 + int F(X(t), t) dt with F a tensor product of cubic B-splines and separate second-difference penalties in the level and time directions. "Functional generalized additive models", Journal of Computational and two arms agree basis function by basis function rather than only in the fit.
}
\value{
A list with components \code{n}, \code{byrow}, \code{estimate}, \code{fitted}, \code{residuals}, \code{coefficients}, \code{intercept}, \code{surface}, \code{surface_x}, \code{edf}, \code{r_squared}, \code{n_x}, \code{n_t}, \code{lam_x}, \code{lam_t}, \code{linear_deviation}, \code{method}, \code{note}.
}
\references{
McLean, M. W., Hooker, G., Staicu, A.-M., Scheipl, F. & Ruppert, D. (2014)
Graphical Statistics 23(1), 249-269, doi:10.1080/10618600.2012.729985.
}
\keyword{internal}
