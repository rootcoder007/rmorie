% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnppvl_moments}
\alias{.bnppvl_moments}
\title{Cellwise first and second moments of the random histogram. The mean}
\usage{
.bnppvl_moments(draws, k)
}
\arguments{
\item{draws}{A vector; its length is taken and its elements indexed.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
of a cell is its midpoint and the second moment is (a^2 + ab + b^2)/3, because the histogram is uniform inside the cell. Taking these in closed form rather than from the sampled positions is what keeps the predictive moments exact given the draws.
}
\value{
A list with \code{m1}, \code{m2}.
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
.bnppvl_moments(draws = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), k = 3L)
}
