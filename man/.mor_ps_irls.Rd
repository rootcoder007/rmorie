% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ps_irls}
\alias{.mor_ps_irls}
\title{.mor_ps_irls}
\usage{
.mor_ps_irls(X, y, lam = 0, max_iter = 200L, tol = 1e-12)
}
\arguments{
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{y}{Passed to \code{.mor_ps_irls_beta}.}
\item{lam}{Passed to \code{.mor_ps_irls_beta}. Defaults to \code{0}.}
\item{max_iter}{Passed to \code{.mor_ps_irls_beta}. Defaults to \code{200L}.}
\item{tol}{Passed to \code{.mor_ps_irls_beta}. Defaults to \code{1e-12}.}
}
\description{
A step of the causal implementation. Called by \code{.fit_propensity}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
