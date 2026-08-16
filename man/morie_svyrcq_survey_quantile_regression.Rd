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
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{tau}{Defaults to \code{0.5}.}
\item{weights}{Defaults to \code{NULL}.}
\item{add_intercept}{Defaults to \code{TRUE}.}
\item{max_iter}{Defaults to \code{200}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{eps}{Defaults to \code{1e-06}.}
}
\description{
Part of the svyrcq_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{residuals}, \code{fitted}, \code{objective}, \code{objective_path}, \code{weighted_fraction_below}, \code{tau}, \code{iterations}, \code{converged}, \code{n}, \code{p}, \code{sum_weights}, \code{method}, \code{note}.
}
\keyword{internal}
