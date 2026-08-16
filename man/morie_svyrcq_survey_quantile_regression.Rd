% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_svyrcq_survey_quantile_regression}
\alias{morie_svyrcq_survey_quantile_regression}
\alias{morie_svyrcq}
\title{Survey-weighted quantile regression by}
\usage{
morie_svyrcq_survey_quantile_regression(X, y, tau = 0.5, weights = NULL, add_intercept = TRUE, max_iter = 200, tol = 1e-10, eps = 1e-06)
}
\description{
R arm of svyrcq -- survey-weighted quantile regression by Complex Surveys, Wiley. Mirrors src/morie/fn/svyrcq.py.
}
\value{
A list with components \code{n}, \code{estimate}, \code{coefficients}, \code{residuals}, \code{fitted}, \code{objective}, \code{objective_path}, \code{weighted_fraction_below}, \code{tau}, \code{iterations}, \code{converged}, \code{p}, \code{sum_weights}, \code{method}, \code{note}.
}
\references{
majorise-minimise. Koenker, R. (2005) Quantile Regression, CUP;
Hunter, D. R. & Lange, K. (2000) JCGS 9(1), 60-77; Lumley, T. (2010)
}
\keyword{internal}
