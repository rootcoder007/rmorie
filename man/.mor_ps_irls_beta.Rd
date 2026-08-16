% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ps_irls_beta}
\alias{.mor_ps_irls_beta}
\title{.mor_ps_irls_beta}
\usage{
.mor_ps_irls_beta(X, y, lam = 0, max_iter = 200L, tol = 1e-12)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{lam}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
A step of the causal implementation. Called by \code{.mor_om_fit_predict}, \code{.mor_ps_irls}. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
