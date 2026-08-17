% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{particle_filter_simple}
\alias{particle_filter_simple}
\title{particle_filter_simple}
\usage{
particle_filter_simple(y, n_particles, init, step, loglik, seed = 0L)
}
\arguments{
\item{y}{See Usage.}
\item{n_particles}{A count; the body uses it as \code{rep(...)}.}
\item{init}{Accepted by the signature and not used anywhere in the body.}
\item{step}{Accepted by the signature and not used anywhere in the body.}
\item{loglik}{Accepted by the signature and not used anywhere in the body.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
}
\description{
A step of the pftrep_native implementation. Called by \code{replicated_pfilter}. See the file header for the source the module follows.
}
\value{
A list with \code{loglik}, \code{min_ess}.
}
\keyword{internal}
\examples{
set.seed(1)
y <- cumsum(rnorm(20, 0, 0.5)) + rnorm(20, 0, 1)
init <- function(n) rnorm(n, 0, 2)
step <- function(parts, t) parts + rnorm(length(parts), 0, 0.5)
loglik <- function(p, obs) dnorm(obs, p, 1, log = TRUE)
pf <- particle_filter_simple(y, 200L, init, step, loglik, seed = 1)
is.list(pf)
}
