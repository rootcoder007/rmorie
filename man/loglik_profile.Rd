% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{loglik_profile}
\alias{loglik_profile}
\title{loglik_profile}
\usage{
loglik_profile(y, grid, make_model, n_particles = 200L, n_reps = 5L, seed = 0L) 
}
\arguments{
\item{y}{Passed to \code{replicated_pfilter}.}
\item{grid}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{make_model}{Accepted by the signature and not used anywhere in the body.}
\item{n_particles}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{n_reps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0L}.}
}
\description{
A step of the pftrep_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{mle}, \code{grid}, \code{loglik}, \code{se}, \code{max_loglik}, \code{n_particles}, \code{n_reps}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
y <- cumsum(rnorm(20, 0, 0.5)) + rnorm(20, 0, 1)
make_model <- function(theta) list(
  init = function(n) rnorm(n, 0, 2),
  step = function(parts, t) parts + rnorm(length(parts), 0, 0.5),
  loglik = function(p, obs) dnorm(obs, p, theta, log = TRUE))
lp <- loglik_profile(y, grid = c(0.5, 1, 2), make_model,
                     n_particles = 150L, n_reps = 3L, seed = 1)
is.list(lp)
}
