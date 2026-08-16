% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlphd_lasso}
\alias{.tmlphd_lasso}
\title{# Cyclic coordinate descent for a weighted lasso; column 1}
\usage{
.tmlphd_lasso(X, y, lam, w = NULL, sweeps = 400L)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{lam}{Passed to \code{.tmlphd_soft}.}
\item{w}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{sweeps}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{400L}.}
}
\description{
unpenalised # Fixed sweep count and fixed cyclic order, so both language arms take # the same path even where the objective has a flat direction.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
\examples{
.tmlphd_lasso(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), lam = c(1, 2, 3, 4, 5, 6, 7, 8))
}
