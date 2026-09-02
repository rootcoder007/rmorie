% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{optimal_rule}
\alias{optimal_rule}
\title{The estimated V-optimal rule, as (d0, d1)}
\usage{
optimal_rule(y, L0, A0, L1, A1, V0 = NULL, V1 = NULL, ridge = 1e-08) 
}
\arguments{
\item{y}{Outcome vector.}
\item{L0}{Covariate matrix at time 0.}
\item{A0}{Treatment at time 0.}
\item{L1}{Covariate matrix at time 1.}
\item{A1}{Treatment at time 1.}
\item{V0}{Summary for the first-stage rule.}
\item{V1}{Summary for the second-stage rule.}
\item{ridge}{Ridge regulariser.}
}
\description{
The estimated V-optimal rule, as (d0, d1)
}
\value{
A list with \code{d0} and \code{d1}.
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
r <- optimal_rule(y, L0, A0, L1, A1)
str(r, max.level = 1)
}
