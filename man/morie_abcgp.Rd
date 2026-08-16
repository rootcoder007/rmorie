% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_abcgp}
\alias{morie_abcgp}
\title{ABC with a Gaussian-process surrogate}
\usage{
morie_abcgp(sim, obs, X_grid = NULL, kernel = "sqexp", method = "wilkinson", prior_ppf = NULL, log_prior = NULL, theta0 = NULL, n_sim = 50, epsilon = 1, summary = NULL, n_waves = 3, n_design = 32, threshold = 10, n_sd = 3, accept_kernel = "gaussian", n_iter = 200, proposal_sd = 0.5, xi = 0.05, delta_s = 10, n_alpha = 64, seed = 0)
}
\arguments{
\item{sim}{Simulator \code{function(theta, e)} where \code{e} is the   shared generator environment.}
\item{obs}{Observed summary.}
\item{X_grid}{Grid of theta to evaluate the emulator on. If NULL the   design is reused.}
\item{kernel}{GP kernel ("sqexp"/"matern32"/"matern52").}
\item{method}{One of "wilkinson", "gps", "adaptive", "synthetic".}
\item{prior_ppf}{For wilkinson: (lo, hi) or list of quantile fns.}
\item{log_prior,theta0,n_sim,epsilon,summary}{For the MH routes.}
\item{n_waves,n_design,threshold,n_sd,accept_kernel}{Wilkinson knobs.}
\item{n_iter,proposal_sd,xi,delta_s,n_alpha}{Meeds & Welling knobs.}
\item{seed}{Seed.}
}
\description{
Four published routes share this name. \code{method="wilkinson"} (the default) emulates the log GABC likelihood of Wilkinson (2014) eq. (1) on a Sobol design pushed through the prior, runs a wave of history-matching to rule out implausible parameter values, and returns the emulator\'s posterior on a grid. \code{method="gps"} and \code{method="adaptive"} are Meeds & Welling (2014) Algorithms 2 and 3: a Metropolis-Hastings sampler in which the accept probability is itself a random variable, drawn from the posterior on the synthetic likelihood\'s mean (their eq. (11)), with tau = median(alpha) and the step repeated until E(alpha) < xi (their eq. (16)). \code{method="synthetic"} is their fixed-S Algorithm 1.
}
\value{
List mirroring the Python RichResult payload.
}
\keyword{internal}
