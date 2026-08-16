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
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{lam}{See Usage.}
\item{outer}{Defaults to \code{15L}.}
\item{sweeps}{Defaults to \code{60L}.}
}
\description{
Part of the tmlphd implementation; see the file header for the source it follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
