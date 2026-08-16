% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{regimen_value}
\alias{regimen_value}
\title{Inverse-probability value of a single-stage rule}
\usage{
regimen_value(R, A, H, rule, propensity = NULL)
}
\arguments{
\item{R}{Numeric vector of outcomes.}
\item{A}{Integer vector of observed treatments, -1 or +1.}
\item{H}{Matrix of covariates.}
\item{rule}{Function mapping a covariate vector to -1 or +1.}
\item{propensity}{Randomisation probabilities.}
}
\description{
Estimates the expected outcome had treatment been assigned by the rule, using inverse-probability weighting.
}
\value{
Numeric scalar, the estimated value of the rule.
}
\keyword{internal}
