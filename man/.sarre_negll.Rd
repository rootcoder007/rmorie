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
\item{lam}{Numeric; combined arithmetically in the body.}
\item{I}{Numeric; combined arithmetically in the body.}
\item{W}{Numeric; combined arithmetically in the body.}
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{y}{A matrix; passed to \code{\%*\%}.}
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
Extracted from the sarre() optimiser closure so the singular-GLS, non-positive-variance and non-positive-determinant guards are all directly unit-testable.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.sarre_negll(lam = c(1, 2, 3, 4, 5, 6, 7, 8), I = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
