% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_fr_eq}
\alias{.groebn_fr_eq}
\title{.groebn_fr_eq}
\usage{
.groebn_fr_eq(p, q)
}
\arguments{
\item{p}{A vector; indexed elementwise.}
\item{q}{A vector; indexed elementwise.}
}
\description{
A step of the groebn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_fr_eq(V, V)
}
