% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{intervention_mechanism}
\alias{intervention_mechanism}
\title{The intervention mechanism}
\usage{
intervention_mechanism(L0, A0, L1, A1, trim = 0.01, known = NULL, penalty = 0)
}
\arguments{
\item{L0}{Covariate matrix at time 0.}
\item{A0}{Treatment at time 0.}
\item{L1}{Covariate matrix at time 1.}
\item{A1}{Treatment at time 1.}
\item{trim}{Truncation lower bound for positivity.}
\item{known}{Optional \code{(p0, p1)} of known probabilities.}
\item{penalty}{Optional lasso penalty (unused unless overridden).}
}
\description{
\code{known} supplies the true probabilities of receiving treatment -- the SMART case, where the design fixes them and nothing needs to be estimated.
}
\value{
\code{g0}, \code{g1}, info with \code{p0}, \code{p1},   \code{min_g0}, \code{min_g1}, \code{max_weight}, \code{known}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 60
L0 <- matrix(rnorm(n), n, 1)
A0 <- rbinom(n, 1, plogis(L0[, 1]))
L1 <- matrix(rnorm(n) + 0.4 * A0, n, 1)
A1 <- rbinom(n, 1, plogis(0.5 * L1[, 1]))
r <- intervention_mechanism(L0, A0, L1, A1)
str(r, max.level = 1)
}
