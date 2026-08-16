% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cox_fit}
\alias{.morie_cox_fit}
\title{.morie_cox_fit}
\usage{
.morie_cox_fit(t, e, X, ties = "efron", max_iter = 50L, tol = 1e-09, offset = NULL)
}
\arguments{
\item{t}{See Usage.}
\item{e}{See Usage.}
\item{X}{See Usage.}
\item{ties}{Defaults to \code{"efron"}.}
\item{max_iter}{Defaults to \code{50L}.}
\item{tol}{Defaults to \code{1e-09}.}
\item{offset}{Defaults to \code{NULL}.}
}
\description{
Part of the cox_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{loglik}, \code{I}, \code{U}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
