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
\item{dist}{Compared against \code{"exponential"}.}
\item{time}{Passed to \code{.ms_check}.}
\item{event}{Passed to \code{.ms_check}.}
\item{X}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the survival_more implementation. Called by \code{Aftfit}, \code{Parasurv}. See the file header for the source the module follows.
}
\value{
A list with \code{dist}, \code{coef}, \code{log_scale}, \code{scale}, \code{loglik}, \code{n_par}, \code{n}, \code{n_events}, \code{aic}, \code{bic}, \code{fixed_scale}, \code{convergence}.
}
\keyword{internal}
