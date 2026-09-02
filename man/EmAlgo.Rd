% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EmAlgo}
\alias{EmAlgo}
\title{\\' Run EM for a fixed number of steps and audit its monotonicity}
\usage{
EmAlgo(log_lik, Q, x0, steps) 
}
\arguments{
\item{steps}{A count; the body uses it as \code{seq_len(...)}.}
\item{log_lik}{Accepted by the signature and not used anywhere in the body.}
\item{Q}{Accepted by the signature and not used anywhere in the body.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
\\' \\' Theorem 1 of Dempster, Laird & Rubin (1977) says every EM step \\' increases the observed-data log likelihood.  That is not assumed but \\' checked: the log likelihood is evaluated at every iterate and the \\' increments are returned along with a monotone flag. \\' \\' @param log_lik function(theta) giving the observed-data log likelihood \\' @param Q function(theta) giving the next iterate (combined E and M step) \\' @param x0 starting parameter vector \\'
}
\value{
A list with \code{theta}, \code{loglik}, \code{trace}, \code{increments}, \code{min_increment}, \code{monotone}, \code{steps}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
xs <- c(rnorm(50, 0), rnorm(50, 4))
ll <- function(mu) sum(log(0.5 * dnorm(xs, mu[1]) +
                           0.5 * dnorm(xs, mu[2])))
Q <- function(mu) {
  r1 <- dnorm(xs, mu[1]); r2 <- dnorm(xs, mu[2])
  g <- r1 / (r1 + r2)
  c(sum(g * xs) / sum(g), sum((1 - g) * xs) / sum(1 - g))
}
r <- EmAlgo(ll, Q, x0 = c(-1, 5), steps = 20)
str(r, max.level = 1)
}
