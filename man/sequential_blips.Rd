% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sequential_blips}
\alias{sequential_blips}
\title{Theorem 22.1: the two blip functions and the V-optimal rule}
\usage{
sequential_blips(y, L0, A0, L1, A1, V0 = NULL, V1 = NULL, ridge = 1e-08, idx = NULL, eval_idx = NULL)
}
\arguments{
\item{y}{Outcome vector (rescaled).}
\item{L0}{Covariate matrix at time 0.}
\item{A0}{Treatment at time 0.}
\item{L1}{Covariate matrix at time 1.}
\item{A1}{Treatment at time 1.}
\item{V0}{Summary for the first-stage rule.}
\item{V1}{Summary for the second-stage rule.}
\item{ridge}{Ridge regulariser.}
\item{idx}{Rows to fit on.}
\item{eval_idx}{Rows to evaluate on.}
}
\description{
Fitted on \code{idx} (all rows by default) and evaluated on \code{eval_idx}. Splitting the two is what makes the cross-validated estimator possible: the rule must not be read off the same rows it is then scored on.
}
\value{
A list with \code{blip1}, \code{blip2}, \code{d0},   \code{d1}, \code{q2}, \code{q1}, \code{coef_q2}, \code{coef_q1},   \code{pseudo}, \code{eval_idx}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 60
L0 <- matrix(rnorm(n), n, 1)
A0 <- rbinom(n, 1, 0.5)
L1 <- matrix(rnorm(n) + 0.4 * A0, n, 1)
A1 <- rbinom(n, 1, 0.5)
y <- plogis(0.3 * A0 + 0.5 * A1 + 0.4 * L0[, 1] + rnorm(n, 0, 0.4))
r <- sequential_blips(y, L0, A0, L1, A1)
str(r, max.level = 1)
}
