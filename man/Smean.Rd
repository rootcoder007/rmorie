% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Smean}
\alias{Smean}
\title{Eq (3.7): mu = (1/N) sum eta(n) -- the DC component of the signal}
\usage{
Smean(x)
}
\arguments{
\item{x}{Passed to \code{.morie_rg_aslist}.}
}
\description{
A step of the rangayyan_ch3 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Smean(V)
}
