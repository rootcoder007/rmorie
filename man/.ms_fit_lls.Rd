% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ms_fit_lls}
\alias{.ms_fit_lls}
\title{.ms_fit_lls}
\usage{
.ms_fit_lls(dist, time, event, X = NULL)
}
\arguments{
\item{dist}{See Usage.}
\item{time}{See Usage.}
\item{event}{See Usage.}
\item{X}{Defaults to \code{NULL}.}
}
\description{
Part of the survival_more implementation; see the file header for the source it follows.
}
\value{
A list with \code{dist}, \code{coef}, \code{log_scale}, \code{scale}, \code{loglik}, \code{n_par}, \code{n}, \code{n_events}, \code{aic}, \code{bic}, \code{fixed_scale}, \code{convergence}.
}
\keyword{internal}
