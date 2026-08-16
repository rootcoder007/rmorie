% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnppvl_conc}
\alias{.bnppvl_conc}
\title{The Beta concentration a_m at a given level. "cubic" is the paper\\'s}
\usage{
.bnppvl_conc(level, c, schedule)
}
\arguments{
\item{level}{Numeric; combined arithmetically in the body.}
\item{c}{See Usage.}
\item{schedule}{One of \code{"constant"}, \code{"cubic"}.}
}
\description{
a_m = c m^3, for which the sum of 1/sqrt(a_m) converges and the limit is absolutely continuous. "constant" holds a_m fixed, which does NOT: the limit is continuous but singular, and it is here because a reader who wants to see that happen should be able to.
}
\value{
Nothing; this branch always raises.
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
