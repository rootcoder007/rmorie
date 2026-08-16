% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnppvl_cell}
\alias{.bnppvl_cell}
\title{The index j in 1..k of the cell holding u, ties to the left cell. A}
\usage{
.bnppvl_cell(u, q, k)
}
\arguments{
\item{u}{See Usage.}
\item{q}{A vector; indexed elementwise.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
linear scan, not a bisection: k is 2^m with m small by construction, and a scan visits the cells in one fixed order in both arms.
}
\value{
The value of \code{k}, as built in the body.
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
.bnppvl_cell(u = 5L, q = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
