% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sschin_cox_breslow}
\alias{.sschin_cox_breslow}
\title{Cox partial likelihood, Breslow ties, Newton-Raphson}
\usage{
.sschin_cox_breslow(t, e, X, max_iter = 100L, tol = 1e-10)
}
\arguments{
\item{t}{See Usage.}
\item{e}{See Usage.}
\item{X}{See Usage.}
\item{max_iter}{Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
Part of the sschin_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{var}, \code{loglik}, \code{iterations}, \code{converged}.
}
\keyword{internal}
