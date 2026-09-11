% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{abcnnt}
\alias{abcnnt}
\alias{sequential_neural_likelihood}
\title{Algorithm 1: Sequential Neural Likelihood}
\usage{
abcnnt(simulator, x_o, log_prior, theta0, n_rounds = 3L, n_per_round = 50L, 
    n_layers = 5L, hidden = 20L, epochs = 40L, lr = 0.01, mcmc_burn = 100L, 
    mcmc_step = 0.5, seed = 0L, n_posterior = 200L) 

sequential_neural_likelihood(simulator, x_o, log_prior, theta0, n_rounds = 3L, n_per_round = 50L, 
    n_layers = 5L, hidden = 20L, epochs = 40L, lr = 0.01, mcmc_burn = 100L, 
    mcmc_step = 0.5, seed = 0L, n_posterior = 200L) 
}
\description{
Algorithm 1: Sequential Neural Likelihood
}
\arguments{
\item{simulator}{Argument \code{simulator}; see Usage.}
\item{x_o}{Argument \code{x_o}; see Usage.}
\item{log_prior}{Argument \code{log_prior}; see Usage.}
\item{theta0}{Argument \code{theta0}; see Usage.}
\item{n_rounds}{Argument \code{n_rounds}; see Usage.}
\item{n_per_round}{Argument \code{n_per_round}; see Usage.}
\item{n_layers}{Argument \code{n_layers}; see Usage.}
\item{hidden}{Argument \code{hidden}; see Usage.}
\item{epochs}{Argument \code{epochs}; see Usage.}
\item{lr}{Argument \code{lr}; see Usage.}
\item{mcmc_burn}{Argument \code{mcmc_burn}; see Usage.}
\item{mcmc_step}{Argument \code{mcmc_step}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
\item{n_posterior}{Argument \code{n_posterior}; see Usage.}
}
\value{
A list with \code{estimate}, \code{posterior_mean}, \code{posterior_sd}, \code{posterior_samples}, \code{flow}, \code{D}, \code{n_simulations}, \code{history}, \code{acceptance}, \code{n_rounds}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
simulator <- function(theta, e) theta * 0.5 + rnorm(length(theta), 0, 0.2)
log_prior <- function(th) if (any(abs(th) > 5)) -Inf else -0.5 * sum(th^2)
set.seed(1)
res <- abcnnt(simulator, x_o = 0.25, log_prior = log_prior, theta0 = 0.5,
              n_rounds = 2L, n_per_round = 25L, epochs = 5L, lr = 0.02,
              seed = 1L, mcmc_burn = 50L, mcmc_step = 0.8)
names(res)
}
