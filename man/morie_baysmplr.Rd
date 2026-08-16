% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_baysmplr}
\alias{morie_baysmplr}
\title{Pick a sampler for this problem and run it}
\usage{
morie_baysmplr(log_p, grad_p = NULL, x0 = NULL, n_iter = 500L, burn = NULL, seed = 1, sampler = NULL, cov_inv = NULL, mean = NULL, eps = 0.1, steps = 10L, scale = NULL, adapt = FALSE, nuts_threshold = 20L, dual_average = FALSE, max_depth = 8L)
}
\arguments{
\item{log_p}{Log density, up to a constant, of a numeric vector.}
\item{grad_p}{Its gradient. Supplying it is what makes HMC and NUTS   available, and the dispatcher will take them when it can.}
\item{x0}{Starting point; its length is the dimension.}
\item{n_iter}{Iterations.}
\item{burn}{Burn-in; half by default.}
\item{seed}{Seed for the generator shared with the Python arm.}
\item{sampler}{Force a sampler instead of dispatching. The reason   field then records that the choice was overridden.}
\item{cov_inv}{A normal target's precision matrix.}
\item{mean}{Its mean, which with cov_inv makes Gibbs available.}
\item{eps}{HMC or NUTS step size.}
\item{steps}{HMC trajectory length.}
\item{scale}{Metropolis proposal scale.}
\item{adapt}{Tune the Metropolis scale during the first half.}
\item{nuts_threshold}{Dimension at or above which NUTS is preferred.}
\item{dual_average}{Tune the NUTS step size during warm-up.}
\item{max_depth}{NUTS doubling limit.}
}
\description{
Pick a sampler for this problem and run it
}
\value{
A list with the chosen sampler and the reason, the posterior   mean and standard deviation per coordinate, the acceptance rate,   the effective sample size and the retained draws.
}
\references{
  Metropolis, N. et al. (1953) J. Chem. Phys. 21(6), 1087-1092.
  Hastings, W.K. (1970) Biometrika 57(1), 97-109.
  Geman, S. and Geman, D. (1984) IEEE PAMI 6(6), 721-741.
  Duane, S. et al. (1987) Physics Letters B 195(2), 216-222.
  Neal, R.M. (2011) "MCMC using Hamiltonian dynamics." Handbook of
    Markov Chain Monte Carlo, chapter 5.
  Hoffman, M.D. and Gelman, A. (2014) "The No-U-Turn Sampler." JMLR
    15, 1593-1623. Algorithms 3 and 6.
  Roberts, G.O., Gelman, A. and Gilks, W.R. (1997) Ann. Appl. Probab.
    7(1), 110-120.
  Geyer, C.J. (1992) Statist. Sci. 7(4), 473-483.
}
\keyword{internal}
