% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnppvl_ab}
\alias{.bnppvl_ab}
\title{.bnppvl_ab}
\usage{
.bnppvl_ab(level, c, schedule, centring, nullq, j)
}
\arguments{
\item{level}{Passed to \code{.bnppvl_conc}.}
\item{c}{Passed to \code{.bnppvl_conc}.}
\item{schedule}{Passed to \code{.bnppvl_conc}.}
\item{centring}{Compared against \code{"uniform"}.}
\item{nullq}{Passed to \code{.bnppvl_null_mean}.}
\item{j}{Passed to \code{.bnppvl_null_mean}.}
}
\description{
A step of the bnppvl_native implementation. Called by \code{morie_bnppvl_draw}, \code{morie_bnppvl_log_prior}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
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
