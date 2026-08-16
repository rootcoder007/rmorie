% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_hurst_rs}
\alias{.morie_hurst_rs}
\title{.morie_hurst_rs}
\usage{
.morie_hurst_rs(x, d = 50L)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{d}{Defaults to \code{50L}.}
}
\description{
Hurst exponent via simple rescaled-range (R/S) analysis -- an exact replica of pracma::hurstexp()$Hs: pad odd-length series to even, truncate to the length in [0.99*N, N] with the most divisors >= 50 (pracma\\'s block-optimal OptN), then compute the whole-series statistic log(R/S) / log(n).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
x <- cumsum(rnorm(4096))
.morie_hurst_rs(x)
}
