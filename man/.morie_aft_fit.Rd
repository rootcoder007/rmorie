% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_aft_fit}
\alias{.morie_aft_fit}
\title{.morie_aft_fit}
\usage{
.morie_aft_fit(t, e, X, family = "weibull", max_iter = 500L, tol = 1e-06, add_intercept = TRUE)
}
\arguments{
\item{t}{See Usage.}
\item{e}{See Usage.}
\item{X}{See Usage.}
\item{family}{Defaults to \code{"weibull"}.}
\item{max_iter}{Defaults to \code{500L}.}
\item{tol}{Defaults to \code{1e-06}.}
\item{add_intercept}{Defaults to \code{TRUE}.}
}
\description{
Part of the aft_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{log_sigma}, \code{loglik}, \code{cov}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
