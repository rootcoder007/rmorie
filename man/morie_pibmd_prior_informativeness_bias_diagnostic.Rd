% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_pibmd_prior_informativeness_bias_diagnostic}
\alias{morie_pibmd_prior_informativeness_bias_diagnostic}
\alias{morie_pibmd}
\title{Prior-data conflict and prior informativeness from}
\usage{
morie_pibmd_prior_informativeness_bias_diagnostic(samples, prior, n_grid = 512L)
}
\description{
R arm of pibmd -- prior-data conflict and prior informativeness from Statistics and Data Analysis, Sec. 3.4.
}
\value{
A list with components \code{mean}, \code{sd}, \code{estimate}, \code{kl_divergence}, \code{kl_divergence_kde}, \code{kl_divergence_reverse}, \code{kl_symmetric}, \code{shrinkage}, \code{bias_in_prior_sd}, \code{wasserstein_1}, \code{conflict_p_value}, \code{conflict_p_value_gaussian}, \code{conflict_p_value_empirical}, \code{posterior_mean}, \code{posterior_var}, \code{posterior_sd}, \code{prior_mean}, \code{prior_var}, \code{prior_sd}, \code{n_posterior}, \code{n_prior}, \code{moments_only}, \code{verdict}, \code{informativeness}, \code{method}, \code{note}.
}
\references{
posterior and prior draws. Evans, M. & Moshonov, H. (2006) Bayesian
Analysis 1(4), 893-914; Kullback, S. & Leibler, R. A. (1951) Ann. Math.
Statist. 22(1), 79-86; Silverman, B. W. (1986) Density Estimation for
}
\keyword{internal}
