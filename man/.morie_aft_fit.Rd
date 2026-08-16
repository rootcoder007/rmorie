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
\item{t}{A vector; its length is taken.}
\item{e}{See Usage.}
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{family}{Passed to \code{.morie_aft_log_dens_surv}. Defaults to \code{"weibull"}.}
\item{max_iter}{Carried through into a list the body builds. Defaults to \code{500L}.}
\item{tol}{Defaults to \code{1e-06}.}
\item{add_intercept}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the aft_native implementation. Called by \code{.morie_aft_common}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{log_sigma}, \code{loglik}, \code{cov}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
