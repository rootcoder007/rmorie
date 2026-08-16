% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tgarch_negll}
\alias{.tgarch_negll}
\title{Internal: GJR-GARCH(1,1) Gaussian negative log-likelihood for the}
\usage{
.tgarch_negll(p, r, n)
}
\arguments{
\item{p}{See Usage.}
\item{r}{See Usage.}
\item{n}{See Usage.}
}
\description{
base-R fallback. Extracted from the morie_tgarch_model() optimiser closure so the parameter-domain guard is directly unit-testable.
}
\value{
A numeric value.
}
\keyword{internal}
