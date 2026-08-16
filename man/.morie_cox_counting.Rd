% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cox_counting}
\alias{.morie_cox_counting}
\title{.morie_cox_counting}
\usage{
.morie_cox_counting(start, stop, event, X, strata = NULL, max_iter = 50L, tol = 1e-09, offset = NULL)
}
\arguments{
\item{start}{See Usage.}
\item{stop}{See Usage.}
\item{event}{See Usage.}
\item{X}{See Usage.}
\item{strata}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{50L}.}
\item{tol}{Defaults to \code{1e-09}.}
\item{offset}{Defaults to \code{NULL}.}
}
\description{
Part of the recur_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{se}, \code{cov}, \code{loglik}, \code{n_iter}, \code{n_events}.
}
\keyword{internal}
