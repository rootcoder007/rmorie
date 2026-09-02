% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnppvl_draw}
\alias{morie_bnppvl_draw}
\title{One draw of the quantile pyramid down to level m}
\usage{
morie_bnppvl_draw(e, m, c = 2.5, schedule = "cubic", centring = "uniform", nullq = NULL) 
}
\arguments{
\item{e}{A random stream from the shared generator.}
\item{m}{Pyramid depth.}
\item{c}{Concentration scale.}
\item{schedule}{A member of the schedule list.}
\item{centring}{A member of the centring list.}
\item{nullq}{The centring quantile function, or NULL.}
}
\description{
Levels are filled in order and, within a level, the odd nodes left to right, so two implementations that consume the same random stream in that order produce the same pyramid.
}
\value{
The full dyadic grid of length 2^m + 1, with 0 and 1 at the   ends.
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
