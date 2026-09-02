% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rule_value_seq}
\alias{rule_value_seq}
\title{Plug-in value of a given rule by sequential regression}
\usage{
rule_value_seq(y, L0, A0, L1, A1, d0, d1, g0, g1, ridge = 1e-08) 
}
\arguments{
\item{y}{Outcome vector (rescaled).}
\item{L0}{Covariate matrix at time 0.}
\item{A0}{Treatment at time 0.}
\item{L1}{Covariate matrix at time 1.}
\item{A1}{Treatment at time 1.}
\item{d0}{First-stage rule.}
\item{d1}{Second-stage rule (two n-long branches).}
\item{g0}{Treatment probability at time 0.}
\item{g1}{Treatment probability at time 1.}
\item{ridge}{Ridge regulariser.}
}
\description{
No targeting: this is the g-computation arm.
}
\value{
Scalar mean outcome.
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
d <- optimal_rule(y, L0, A0, L1, A1)
im <- intervention_mechanism(L0, A0, L1, A1)
rule_value_seq(y, L0, A0, L1, A1, d$d0, d$d1, im$g0, im$g1)
}
