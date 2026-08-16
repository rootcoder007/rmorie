% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnppvl_null_mean}
\alias{.bnppvl_null_mean}
\title{The mean of V that centres the pyramid on a prior guess: the fraction}
\usage{
.bnppvl_null_mean(nullq, j, level)
}
\arguments{
\item{nullq}{Accepted by the signature and not used anywhere in the body.}
\item{j}{Numeric; combined arithmetically in the body.}
\item{level}{Passed to \code{bitwShiftL}.}
}
\description{
of the parent interval that the guess assigns to the left child, which is the paper\\'s equation (6). A guess that is flat over the parent gives exactly a half, the symmetric case.
}
\value{
A numeric value.
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
