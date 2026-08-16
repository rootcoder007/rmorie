% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CoxPL}
\alias{CoxPL}
\title{\\' Cox partial likelihood, its score and information, optionally}
\usage{
CoxPL(time, event, X, beta = NULL, max_iter = 50L, tol = 1e-10)
}
\arguments{
\item{max_iter,tol}{Newton-Raphson controls, used only when beta is NULL \\' @return list(loglik, score, information, coefficients, vcov, se, n, \\' n_event, iterations, converged, method) \\' @export }
\item{time}{A vector; its length is taken.}
\item{event}{A vector; its length is taken.}
\item{X}{A matrix; passed to \code{nrow}.}
\item{beta}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{max_iter}{Defaults to \code{50L}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
fitted \\' \\' Breslow handling of tied event times.  When beta is NULL the partial \\' likelihood is maximised by Newton-Raphson from zero. Checked against \\' survival::coxph(ties = "breslow"). \\' \\' @param time follow-up times \\' @param event 1 for an observed event, 0 for right censoring \\' @param X covariate matrix, no intercept column \\' @param beta coefficients to evaluate at, or NULL to fit \\'
}
\value{
A list with \code{loglik}, \code{score}, \code{information}, \code{coefficients}, \code{vcov}, \code{se}, \code{n}, \code{n_event}, \code{iterations}, \code{converged}, \code{method}.
}
\keyword{internal}
\examples{
CoxPL(time = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5), event = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8))
}
