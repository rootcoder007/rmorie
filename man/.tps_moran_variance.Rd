% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tps_moran_variance}
\alias{.tps_moran_variance}
\title{Internal helper: Tps Moran Variance}
\usage{
.tps_moran_variance(W, n, z = NULL, randomisation = TRUE)
}
\arguments{
\item{W}{Spatial weights matrix.}
\item{n}{Number of areal units.}
\item{z}{Centred values (\eqn{x - \bar{x}}); required for the   randomisation branch, ignored under normality.}
\item{randomisation}{Use the randomisation null (default) or the   normality null.}
}
\description{
Variance of Moran\'s I under either null, following Cliff & Ord (1981). With \eqn{E(I) = -1/(n-1)}: Normality -- the values are draws from a normal population, so only the weights matter: \deqn{Var_N(I) = \frac{n^2 S_1 - n S_2 + 3 S_0^2}{S_0^2 (n^2 - 1)}   - E(I)^2.} Randomisation -- the observed values are fixed and only their ASSIGNMENT to locations is random, so the sample kurtosis \eqn{b_2 = n \sum z_i^4 / (\sum z_i^2)^2} enters: \deqn{Var_R(I) = \frac{n\left[(n^2-3n+3)S_1 - n S_2 + 3S_0^2\right]   - b_2\left[(n^2-n)S_1 - 2n S_2 + 6S_0^2\right]}   {(n-1)(n-2)(n-3)S_0^2} - E(I)^2.} Randomisation is the default because it is the weaker assumption and these are incident COUNTS -- skewed, non-negative, nothing like normal. Under heavy kurtosis the two disagree materially, and the normal-theory p-value is the optimistic one. Both branches reproduce \code{spdep::moran.test()} at its matching \code{randomisation} setting (tests/cross/test-morie_vs_spatial.R). A kurtosis large enough to drive the variance negative is reported rather than silently returned, since the test does not hold there.
}
\value{
Named list with \code{variance}, \code{kurtosis} and   \code{assumption}.
}
\keyword{internal}
