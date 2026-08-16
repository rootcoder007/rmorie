% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpErrMod}
\alias{SpErrMod}
\title{SAR ERROR model, eqs (6.35)-(6.37) of Sec. 6.2.2.1 -- NOT the}
\usage{
SpErrMod(x, y, w, n_grid = 201L, refine = 60L)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{w}{See Usage.}
\item{n_grid}{Defaults to \code{201L}.}
\item{refine}{Defaults to \code{60L}.}
}
\description{
spatially lagged model of eq (6.38).  Whitening by A = I - rho W profiles beta and sigma^2 out, so only a ONE-dimensional search in rho remains; the -2 log|A| Jacobian is what makes naive OLS-in-rho wrong.
}
\value{
A list with \code{rho}, \code{beta}, \code{sigma2}, \code{neg2loglik}, \code{rho_bounds}, \code{ols_beta}, \code{spectral_radius}, \code{is_error_model_not_lag_model}, \code{k}, \code{n}, \code{method}.
}
\keyword{internal}
