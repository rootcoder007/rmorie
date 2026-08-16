% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_slbpdg}
\alias{morie_slbpdg}
\title{Slice-sampled Dirichlet-process mixture of normals}
\usage{
morie_slbpdg(y, alpha = 1, n_iter = 500L, burn = NULL, thin = 1L, route = "walker", kappa = 0.5, m0 = NULL, kappa0 = 0.01, a0 = 2, b0 = NULL, seed = 1, max_components = 200L, grid = NULL, alpha_update = NULL, alpha_a = 2, alpha_b = 1, keep_draws = FALSE)
}
\arguments{
\item{y}{The data.}
\item{alpha}{Dirichlet-process concentration; the starting value when   it is being updated.}
\item{n_iter}{Total sweeps, including burn-in.}
\item{burn}{Sweeps discarded; half of n_iter by default.}
\item{thin}{Keep every thin-th sweep after burn-in.}
\item{route}{"walker" for the random bound u_i below the weight of   the point's own component, or "kalli_griffin_walker" for the   deterministic xi_k.}
\item{kappa}{The geometric rate of xi_k, used only by the second   route. Smaller carries fewer components and mixes worse.}
\item{m0}{Prior mean; the sample mean by default.}
\item{kappa0}{Prior precision scaling on the mean.}
\item{a0}{Inverse-gamma shape.}
\item{b0}{Inverse-gamma scale; set from the sample variance by   default, which puts the prior on the scale of the data instead of   on the scale of whatever units it arrived in.}
\item{seed}{Seed for the generator shared with the Python arm.}
\item{max_components}{A hard ceiling on the components carried in one   sweep. Reaching it is reported in hit_ceiling rather than silently   truncating the model into a different one.}
\item{grid}{Points at which the posterior mean density is   accumulated.}
\item{alpha_update}{NULL to hold alpha fixed, or "escobar_west".}
\item{alpha_a}{Shape of the gamma prior on alpha.}
\item{alpha_b}{Rate of that prior.}
\item{keep_draws}{Also return the component state of every retained   sweep -- weights, means and variances. Downstream modules needing a   posterior of some functional of the mixture (a quantile, a tail   probability) work from these rather than re-running a sampler of   their own.}
}
\description{
Slice-sampled Dirichlet-process mixture of normals
}
\value{
A list of posterior summaries: the mean density on the grid,   the distribution of the number of occupied clusters, the retained   alpha draws and per-sweep diagnostics.
}
\references{
  Walker, S.G. (2007) "Sampling the Dirichlet mixture model with
    slices." Communications in Statistics -- Simulation and
    Computation 36(1), 45-54.
  Kalli, M., Griffin, J.E. and Walker, S.G. (2011) "Slice sampling
    mixture models." Statistics and Computing 21(1), 93-105.
  Escobar, M.D. and West, M. (1995) "Bayesian density estimation and
    inference using mixtures." JASA 90(430), 577-588, section 6.
  Ishwaran, H. and James, L.F. (2001) "Gibbs sampling methods for
    stick-breaking priors." JASA 96(453), 161-173.
}
\keyword{internal}
