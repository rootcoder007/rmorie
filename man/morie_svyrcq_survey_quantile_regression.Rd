% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_svyrcq_survey_quantile_regression}
\alias{morie_svyrcq_survey_quantile_regression}
\title{morie_svyrcq_survey_quantile_regression}
\usage{
morie_svyrcq_survey_quantile_regression(X, y, tau = 0.5, weights = NULL, add_intercept = TRUE, max_iter = 200, tol = 1e-10, eps = 1e-06)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{tau}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{add_intercept}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{eps}{Defaults to \code{1e-06}.}
}
\description{
A step of the svyrcq_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{residuals}, \code{fitted}, \code{objective}, \code{objective_path}, \code{weighted_fraction_below}, \code{tau}, \code{iterations}, \code{converged}, \code{n}, \code{p}, \code{sum_weights}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_svyrcq_survey_quantile_regression(V, V)
}
