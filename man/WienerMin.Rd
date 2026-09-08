% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerMin}
\alias{WienerMin}
\title{Eq. (3.172): J_min = var(d) - Theta\' w_o.  A negative J_min cannot}
\usage{
WienerMin(phi, theta, var_d) 
}
\arguments{
\item{phi}{Passed to \code{WienerHopf}.}
\item{theta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{var_d}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
happen for consistent statistics, so it is reported rather than clamped: it means the supplied variance and covariances do not come from the same process.
}
\value{
A list with \code{j_min}, \code{w_o}, \code{var_d}, \code{explained}, \code{consistent}, \code{fraction_explained}, \code{method}.
}
\keyword{internal}
\examples{
WienerMin(phi = 0.5, theta = 0.5, var_d = 5L)
}
