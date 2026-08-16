% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{abcnnt}
\alias{abcnnt}
\title{Algorithm 1: Sequential Neural Likelihood}
\usage{
abcnnt(simulator, x_o, log_prior, theta0, n_rounds = 3L, n_per_round = 50L, n_layers = 5L, hidden = 20L, epochs = 40L, lr = 0.01, mcmc_burn = 100L, mcmc_step = 0.5, seed = 0L, n_posterior = 200L)
}
\description{
Algorithm 1: Sequential Neural Likelihood
}
\value{
A list with \code{estimate}, \code{posterior_mean}, \code{posterior_sd}, \code{posterior_samples}, \code{flow}, \code{D}, \code{n_simulations}, \code{history}, \code{acceptance}, \code{n_rounds}, \code{method}, \code{note}.
}
\keyword{internal}
