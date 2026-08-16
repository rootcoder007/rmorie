% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_lcm}
\alias{.groebn_lcm}
\title{.groebn_lcm}
\usage{
.groebn_lcm(a, b)
}
\arguments{
\item{a}{Passed to \code{pmax}.}
\item{b}{Passed to \code{pmax}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_spoly}. See the file header for the source the module follows.
}
\value{
The value of \code{pmax}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_lcm(V, V)
}
