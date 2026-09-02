% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{multiplier_bootstrap}
\alias{multiplier_bootstrap}
\title{multiplier_bootstrap}
\usage{
multiplier_bootstrap(ic, B = 1000L, seed = 0L) 
}
\arguments{
\item{ic}{Passed to \code{morie_tlboot}.}
\item{B}{Passed to \code{morie_tlboot}. Defaults to \code{1000L}.}
\item{seed}{Passed to \code{morie_tlboot}. Defaults to \code{0L}.}
}
\description{
A step of the tlboot_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{morie_tlboot}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
multiplier_bootstrap(V)
}
