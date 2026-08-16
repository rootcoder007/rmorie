% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnppvl_loglik}
\alias{morie_bnppvl_loglik}
\title{The log likelihood of the cell configuration}
\usage{
morie_bnppvl_loglik(u, q, kind = "exact")
}
\arguments{
\item{u}{Observations mapped onto the unit interval.}
\item{q}{The dyadic grid.}
\item{kind}{A member of the likelihood list.}
}
\description{
The exact version is the product of the random-histogram densities. The substitute version is the multinomial probability of the counts under equal cell probabilities, which does not depend on the widths at all -- and that is precisely the property that makes it conservative.
}
\value{
The log likelihood.
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
