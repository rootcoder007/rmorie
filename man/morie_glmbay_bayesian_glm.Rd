% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_glmbay_bayesian_glm}
\alias{morie_glmbay_bayesian_glm}
\alias{morie_glmbay}
\title{Bayesian GLM by penalised IRLS to the posterior mode}
\usage{
morie_glmbay_bayesian_glm(X, y, family = "binomial", prior_sd = 2.5, add_intercept = TRUE, max_iter = 100, tol = 1e-10)
}
\description{
R arm of glmbay -- Bayesian GLM by penalised IRLS to the posterior mode Data Analysis, 3rd ed., CRC Press, Ch. 16 and Sec. 4.1.
}
\value{
A list with components \code{n}, \code{estimate}, \code{coefficients}, \code{posterior_sd}, \code{std_error}, \code{ci_lower}, \code{ci_upper}, \code{fitted}, \code{linear_predictor}, \code{loglik}, \code{log_prior}, \code{log_marginal}, \code{log_det_hessian}, \code{iterations}, \code{converged}, \code{family}, \code{prior_sd}, \code{p}, \code{method}, \code{note}.
}
\references{
with a normal prior, Laplace covariance. Gelman, A. et al. (2013) Bayesian
}
\keyword{internal}
