% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarre_negll}
\alias{.sarre_negll}
\title{Internal: SAR-error concentrated negative log-likelihood in lambda}
\usage{
.sarre_negll(lam, I, W, X, y, n)
}
\arguments{
\item{lam}{See Usage.}
\item{I}{See Usage.}
\item{W}{See Usage.}
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{n}{See Usage.}
}
\description{
Extracted from the sarre() optimiser closure so the singular-GLS, non-positive-variance and non-positive-determinant guards are all directly unit-testable.
}
\value{
A numeric value.
}
\keyword{internal}
