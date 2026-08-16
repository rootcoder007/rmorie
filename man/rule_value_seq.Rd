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
