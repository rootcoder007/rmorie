% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.garch_negll}
\alias{.garch_negll}
\title{Internal: GARCH(1,1) Gaussian negative log-likelihood for the base-R}
\usage{
.garch_negll(p, r, n)
}
\arguments{
\item{p}{A vector; indexed elementwise.}
\item{r}{A vector; indexed elementwise.}
\item{n}{A count; the body uses it as \code{numeric(...)}.}
}
\description{
fallback. Extracted from the morie_garch_fit() optimiser closure so the parameter-domain guard is directly unit-testable.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.garch_negll(p = c(1, 2, 3, 4, 5, 6, 7, 8), r = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
