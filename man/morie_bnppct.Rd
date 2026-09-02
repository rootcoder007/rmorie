% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnppct}
\alias{morie_bnppct}
\title{Posterior of the quantile function of a nonparametric F}
\usage{
morie_bnppct(y, quantile = 0.5, route = "mixture", alpha = 1, n_iter = 500L, 
    burn = NULL, thin = 1L, seed = 1, cred = 0.9, sampler_route = "walker", 
    kappa = 0.5, m0 = NULL, kappa0 = 0.01, a0 = 2, b0 = NULL, n_bootstrap = 500L, 
    alpha_update = NULL) 
}
\arguments{
\item{y}{The data.}
\item{quantile}{One or more probabilities in (0, 1).}
\item{route}{"mixture", "predictive" or "bayesian_bootstrap".}
\item{alpha}{Dirichlet-process concentration for the mixture routes.}
\item{n_iter}{Sampler length.}
\item{burn}{Burn-in.}
\item{thin}{Thinning.}
\item{seed}{Seed for the generator shared with the Python arm.}
\item{cred}{Credible level for the reported interval.}
\item{sampler_route}{Which slice sampler morie_slbpdg should use.}
\item{kappa}{Geometric rate for the Kalli-Griffin-Walker route.}
\item{m0}{Prior mean.}
\item{kappa0}{Prior precision scaling on the mean.}
\item{a0}{Inverse-gamma shape.}
\item{b0}{Inverse-gamma scale.}
\item{n_bootstrap}{Replicates for the Bayesian-bootstrap route.}
\item{alpha_update}{Passed through to the sampler.}
}
\description{
Posterior of the quantile function of a nonparametric F
}
\value{
A list with, per quantile, the posterior mean, standard   deviation, median and credible bounds, plus the draws themselves.
}
\references{
  Kottas, A. and Krnjajic, M. (2009) "Bayesian semiparametric
    modelling in quantile regression." Scandinavian Journal of
    Statistics 36(2), 297-319.
  Rubin, D.B. (1981) "The Bayesian bootstrap." Annals of Statistics
    9(1), 130-134.
  Walker (2007); Kalli, Griffin and Walker (2011) -- the sampler,
    through morie_slbpdg.
}
\keyword{internal}
\examples{
N <- 60L
ii <- 0:(N - 1L)
Y <- exp(0.45 * sin(2.7 * ii) + 0.3 * cos(0.6 * ii)) + 0.15 * 
    (ii\%\%4L)
QS <- c(0.1, 0.5, 0.9)
morie_bnppct(Y, QS, route = "mixture", n_iter = 70L, burn = 50L, 
    seed = 7)
}
