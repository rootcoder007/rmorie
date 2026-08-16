% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_fr_neg}
\alias{.groebn_fr_neg}
\title{.groebn_fr_neg}
\usage{
.groebn_fr_neg(p)
}
\arguments{
\item{p}{A vector; indexed elementwise.}
}
\description{
A step of the groebn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_fr_neg(V)
}
