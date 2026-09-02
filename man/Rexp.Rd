% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rexp}
\alias{Rexp}
\title{Rexp}
\usage{
Rexp(n, rate = 1, seed = 0, stream = 0) 
}
\arguments{
\item{n}{Passed to \code{.morie_random_uniform}.}
\item{rate}{Passed to \code{Qexp}. Defaults to \code{1}.}
\item{seed}{Passed to \code{.morie_random_uniform}. Defaults to \code{0}.}
\item{stream}{Passed to \code{.morie_random_uniform}. Defaults to \code{0}.}
}
\description{
A step of the dist_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{Qexp}.
}
\keyword{internal}
\examples{
Rexp(n = 5L)
}
