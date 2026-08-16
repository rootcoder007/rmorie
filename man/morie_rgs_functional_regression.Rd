% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_rgs_functional_regression}
\alias{morie_rgs_functional_regression}
\alias{morie_rgs}
\title{The functional linear model Y = int beta(t) X(t) dt by}
\usage{
morie_rgs_functional_regression(X, Y, basis = NULL)
}
\description{
R arm of rgs -- the functional linear model Y = int beta(t) X(t) dt by principal-component truncation. Statistics & Probability Letters 45(1), 11-22, trapezoid weights so the eigenproblem is the one for the INTEGRAL operator rather than for the raw matrix; beta is divided by the eigenvalues, so the truncation level k is the estimator and not a tuning detail.
}
\value{
A list with components \code{n}, \code{symmetric}, \code{decreasing}, \code{drop}, \code{estimate}, \code{beta}, \code{fitted}, \code{residuals}, \code{k}, \code{eigenvalues}, \code{explained}, \code{scores}, \code{mean_curve}, \code{r_squared}, \code{n_grid}, \code{method}, \code{note}.
}
\references{
Cardot, H., Ferraty, F. & Sarda, P. (1999) "Functional linear model",
doi:10.1016/S0167-7152(99)00036-X.
}
\keyword{internal}
