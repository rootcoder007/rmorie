% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_mcmc_autocov}
\alias{.morie_mcmc_autocov}
\title{Autocovariance by FFT, matching _mcmc.autocov: zero-padded to a power}
\usage{
.morie_mcmc_autocov(x, max_lag = NULL)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{max_lag}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
of two at least 2n, divided by n (biased, which is what keeps the sequence positive semi-definite).
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_mcmc_autocov(V)
}
