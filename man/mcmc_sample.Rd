% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mcmc_sample}
\alias{mcmc_sample}
\title{Random-walk Metropolis, used to draw from the current posterior}
\usage{
mcmc_sample(logpdf, x0, n, burn = 100L, step = 0.5, seed = 0L) 
}
\description{
Random-walk Metropolis, used to draw from the current posterior
}
\arguments{
\item{logpdf}{Argument \code{logpdf}; see Usage.}
\item{x0}{Argument \code{x0}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{burn}{Argument \code{burn}; see Usage.}
\item{step}{Argument \code{step}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
}
\value{
A list with \code{samples}, \code{acceptance}.
}
\keyword{internal}
\examples{
samples <- mcmc_sample(function(x) -0.5 * sum(x^2), x0 = 0, n = 200,
                       burn = 50, seed = 1L)
mean(samples)
}
