% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnppvl_empirical_start}
\alias{.bnppvl_empirical_start}
\title{A starting pyramid halfway between the data and the uniform one. The}
\usage{
.bnppvl_empirical_start(u, k)
}
\arguments{
\item{u}{A vector; its length is taken.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
empirical quantiles alone will not do: with ties, or with fewer observations than cells, two of them can coincide and a quantile function with a zero-width cell is not a quantile function at all. Averaging with the uniform grid guarantees a strict increase of at least one over twice the cell count while still starting the chain where the data are, which is worth a great many sweeps of a single-site sampler.
}
\value{
The value of \code{q}, as built in the body.
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
