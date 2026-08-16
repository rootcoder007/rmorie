% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{moment_estimate}
\alias{moment_estimate}
\title{moment_estimate}
\usage{
moment_estimate(rho)
}
\arguments{
\item{rho}{Passed to \code{.sarima_invert_rho}.}
}
\description{
A step of the sarima_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.sarima_invert_rho}.
}
\keyword{internal}
\examples{
moment_estimate(rho = 0.5)
}
