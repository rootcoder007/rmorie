% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_invert_rho}
\alias{.sarima_invert_rho}
\title{.sarima_invert_rho}
\usage{
.sarima_invert_rho(rho)
}
\arguments{
\item{rho}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the sarima_native implementation. Called by \code{moment_estimate}, \code{preliminary_estimates}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
