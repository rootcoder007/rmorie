% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnppvl}
\alias{morie_bnppvl}
\title{Fit a Beta quantile pyramid and report the predictive for a new draw}
\usage{
morie_bnppvl(x, m = 4L, c = 2.5, schedule = "cubic", centring = "uniform", nullq = NULL, likelihood = "exact", lo = 0, hi = 1, sweeps = 400L, burn = 100L, thin = 2L, seed = 0, init = "prior", grid = NULL, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
}
\arguments{
\item{x}{The observations, which must lie inside the support.}
\item{m}{Pyramid depth; the histogram has k = 2^m cells.}
\item{c}{Concentration scale. Larger holds the pyramid closer to its   centring.}
\item{schedule}{A member of the schedule list.}
\item{centring}{A member of the centring list; "null" centres on the   given quantile function.}
\item{nullq}{The centring quantile function on the unit interval,   strictly increasing, or NULL.}
\item{likelihood}{A member of the likelihood list.}
\item{lo}{Lower end of the support.}
\item{hi}{Upper end of the support.}
\item{sweeps}{Metropolis-Hastings sweeps in total.}
\item{burn}{Sweeps discarded.}
\item{thin}{Keep-every rate.}
\item{seed}{The random stream.}
\item{init}{A member of the init list. "prior" starts the chain at a   draw from the prior, which is what the paper describes;   "empirical" starts it near the data, which reaches the same   posterior in far fewer sweeps of a single-site sampler.}
\item{grid}{Points at which the predictive density and distribution   function are reported, or NULL.}
\item{probs}{Predictive quantile levels.}
}
\description{
Fit a Beta quantile pyramid and report the predictive for a new draw
}
\value{
A list with the posterior mean quantile function, the   predictive mean and standard deviation of a new observation, the   predictive density and distribution function on the grid, the   predictive quantiles, and the acceptance rate.
}
\references{
  Hjort, N.L. and Walker, S.G. (2009) "Quantile pyramids for Bayesian
    nonparametrics." The Annals of Statistics 37(1), 105-131.
    doi:10.1214/07-AOS553. The construction (equation 3), the Beta
    pyramid and the a_m = c m^3 schedule (section 4.1), the centring
    identity (equation 6), the random-histogram density (equation 9),
    the exact likelihood (equation 10), the multinomial substitute
    likelihood (equation 11), the factorised prior (equation 15) and
    both Metropolis-Hastings acceptance ratios (section 6).
  Jeffreys, H. (1967) "Theory of Probability," 3rd edition. Oxford
    University Press, chapter 4.
  Lavine, M. (1995) "On an approximate likelihood for quantiles."
    Biometrika 82(1), 220-222.
  Ferguson, T.S. (1974) "Prior distributions on spaces of probability
    measures." The Annals of Statistics 2(4), 615-629.
}
\keyword{internal}
\examples{
X <- vapply(0:39, function(i) {
    v <- ((i * 37)%%101)/100
    v * v
}, numeric(1))
morie_bnppvl(X, m = 3L, c = 2.5, likelihood = "exact", sweeps = 400L, 
    burn = 100L, thin = 4L, seed = 7, init = "empirical")
}
