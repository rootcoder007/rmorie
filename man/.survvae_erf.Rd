% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.survvae_erf}
\alias{.survvae_erf}
\title{Base R has no erf/erfc; both are pnorm in disguise. Defined here so}
\usage{
.survvae_erf(x)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
}
\description{
the arm stays base-R only, as the package requires.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.survvae_erf(V)
}
