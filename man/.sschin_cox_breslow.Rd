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
\item{t}{A vector; its length is taken and its elements indexed.}
\item{e}{A vector; indexed elementwise.}
\item{X}{A matrix; indexed by row and column.}
\item{max_iter}{Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
A step of the sschin_native implementation. Called by \code{morie_sschin_chained_imputation}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{var}, \code{loglik}, \code{iterations}, \code{converged}.
}
\keyword{internal}
