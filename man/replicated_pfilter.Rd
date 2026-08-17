% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{replicated_pfilter}
\alias{replicated_pfilter}
\title{replicated_pfilter}
\usage{
replicated_pfilter(y, n_particles, init, step, loglik, n_reps = 10L, seed = 0L)
}
\arguments{
\item{y}{Passed to \code{particle_filter_simple}.}
\item{n_particles}{Coerced to integer by the body, with \code{as.integer}.}
\item{init}{Passed to \code{particle_filter_simple}.}
\item{step}{Passed to \code{particle_filter_simple}.}
\item{loglik}{Passed to \code{particle_filter_simple}.}
\item{n_reps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10L}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0L}.}
}
\description{
A step of the pftrep_native implementation. Called by \code{loglik_profile}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{loglik}, \code{logmeanexp}, \code{mean_loglik}, \code{jensen_gap}, \code{se}, \code{replicates}, \code{n_reps}, \code{n_particles}, \code{min_ess}, \code{mean_min_ess}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
y <- cumsum(rnorm(20, 0, 0.5)) + rnorm(20, 0, 1)
init <- function(n) rnorm(n, 0, 2)
step <- function(parts, t) parts + rnorm(length(parts), 0, 0.5)
loglik <- function(p, obs) dnorm(obs, p, 1, log = TRUE)
r <- replicated_pfilter(y, 200L, init, step, loglik, n_reps = 5L, seed = 1)
is.list(r)
}
