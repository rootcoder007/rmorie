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
\item{x}{A matrix; passed to \code{as.matrix}.}
\item{y}{Passed to \code{.morie_spx_chkv}.}
\item{w}{Passed to \code{.morie_spx_chkw}.}
\item{n_grid}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{201L}.}
\item{refine}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60L}.}
}
\description{
spatially lagged model of eq (6.38).  Whitening by A = I - rho W profiles beta and sigma^2 out, so only a ONE-dimensional search in rho remains; the -2 log|A| Jacobian is what makes naive OLS-in-rho wrong.
}
\value{
A list with \code{rho}, \code{beta}, \code{sigma2}, \code{neg2loglik}, \code{rho_bounds}, \code{ols_beta}, \code{spectral_radius}, \code{is_error_model_not_lag_model}, \code{k}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
W <- matrix(0, 12, 12)
W[abs(row(W) - col(W)) == 1] <- 1
x <- rnorm(12)
y <- 1 + 2 * x + rnorm(12, 0, 0.3)
str(SpErrMod(x, y, W), max.level = 1)
}
