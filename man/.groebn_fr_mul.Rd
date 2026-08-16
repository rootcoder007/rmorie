% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_fr_mul}
\alias{.groebn_fr_mul}
\title{.groebn_fr_mul}
\usage{
.groebn_fr_mul(p, q)
}
\arguments{
\item{p}{A vector; indexed elementwise.}
\item{q}{A vector; indexed elementwise.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_mul}, \code{.groebn_scale}. See the file header for the source the module follows.
}
\value{
The value of \code{.groebn_fr}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_fr_mul(V, V)
}
