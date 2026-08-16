% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlphd_lasso_logit}
\alias{.tmlphd_lasso_logit}
\title{# Proximal-Newton L1 logistic: IRLS outside, weighted lasso inside}
\usage{
.tmlphd_lasso_logit(X, y, lam, outer = 15L, sweeps = 60L)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{lam}{Passed to \code{.tmlphd_lasso}.}
\item{outer}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{15L}.}
\item{sweeps}{Passed to \code{.tmlphd_lasso}. Defaults to \code{60L}.}
}
\description{
A step of the tmlphd implementation. Called by \code{Tmlphd}. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
