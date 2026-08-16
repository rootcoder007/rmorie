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
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{strata}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.}
\item{tol}{Defaults to \code{1e-09}.}
\item{offset}{Defaults to \code{NULL}.}
}
\description{
A step of the recur_native implementation. Called by \code{Agrec}, \code{Pwpgt}, \code{Shfrm} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{se}, \code{cov}, \code{loglik}, \code{n_iter}, \code{n_events}.
}
\keyword{internal}
