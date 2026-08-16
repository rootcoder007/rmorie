% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dccmd_negll}
\alias{.dccmd_negll}
\title{Internal: DCC(1,1) two-step Gaussian negative log-likelihood for the}
\usage{
.dccmd_negll(p, Q_bar, n, Z)
}
\arguments{
\item{p}{A vector; indexed elementwise.}
\item{Q_bar}{Numeric; combined arithmetically in the body.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{Z}{A matrix; indexed by row and column.}
}
\description{
base-R fallback path. Extracted from the morie_dcc_multivariate_garch() optimiser closure so the parameter-domain and non-positive-determinant guards are directly unit-testable. `Q_bar` is the unconditional correlation, `n` the sample size, `Z` the standardised residuals.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
.dccmd_negll(p = c(1, 2, 3, 4, 5, 6, 7, 8), Q_bar = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, Z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
