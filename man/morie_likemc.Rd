% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_likemc}
\alias{morie_likemc}
\title{morie_likemc}
\usage{
morie_likemc(model, data, priors, n_iter, seed = 1, step = 0.15, burn = 0)
}
\arguments{
\item{model}{See Usage.}
\item{data}{See Usage.}
\item{priors}{See Usage.}
\item{n_iter}{See Usage.}
\item{seed}{Defaults to \code{1}.}
\item{step}{Defaults to \code{0.15}.}
\item{burn}{Defaults to \code{0}.}
}
\description{
Part of the likemc_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{beta_mean}, \code{gamma_mean}, \code{chain}, \code{n_draws}, \code{n_iter}, \code{acceptance_rate}, \code{R0_mean}, \code{R0_q025}, \code{R0_median}, \code{R0_q975}, \code{logpost_final}, \code{seed}, \code{step}, \code{method}.
}
\keyword{internal}
