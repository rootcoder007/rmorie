% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.drbsze_tquant}
\alias{.drbsze_tquant}
\title{Two-sided Student-t quantile by Cornish-Fisher expansion on the}
\usage{
.drbsze_tquant(p, df)
}
\arguments{
\item{p}{Passed to \code{.s03qnorm}.}
\item{df}{Numeric; combined arithmetically in the body.}
}
\description{
normal Part of the Drbsze implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.drbsze_tquant(V, V)
}
