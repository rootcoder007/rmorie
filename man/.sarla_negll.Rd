% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarla_negll}
\alias{.sarla_negll}
\title{Internal: SAR-lag concentrated negative log-likelihood in rho}
\usage{
.sarla_negll(rho, e0, e1, n, I, W)
}
\arguments{
\item{rho}{Numeric; combined arithmetically in the body.}
\item{e0}{Numeric; combined arithmetically in the body.}
\item{e1}{Numeric; combined arithmetically in the body.}
\item{n}{Numeric; combined arithmetically in the body.}
\item{I}{Numeric; combined arithmetically in the body.}
\item{W}{Numeric; combined arithmetically in the body.}
}
\description{
Extracted from the sarla() optimiser closure so the singular-system / non-positive-variance guard is directly unit-testable. `e0`, `e1` are the residual projections, `n` the sample size, `I`/`W` the identity and spatial-weights matrices.
}
\value{
A numeric value.
}
\keyword{internal}
