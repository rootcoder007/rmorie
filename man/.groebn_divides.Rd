% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_divides}
\alias{.groebn_divides}
\title{.groebn_divides}
\usage{
.groebn_divides(a, b)
}
\arguments{
\item{a}{Passed to \code{<=}.}
\item{b}{Passed to \code{<=}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_divide}, \code{.groebn_reduce_basis}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_divides(V, V)
}
