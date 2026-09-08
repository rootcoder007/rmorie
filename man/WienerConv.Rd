% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerConv}
\alias{WienerConv}
\title{Eqs. (3.173)-(3.174): the normal equations written as a convolution}
\usage{
WienerConv(w, phi, theta) 
}
\arguments{
\item{w}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{phi}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{theta}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
of the tap weights with the ACF.  It holds only for a stationary process; that premise is recorded because nothing in the arithmetic checks it.
}
\value{
A list with \code{lhs}, \code{theta}, \code{max_difference}, \code{holds}, \code{order}, \code{requires_stationarity}, \code{method}.
}
\keyword{internal}
\examples{
WienerConv(w = 5L, phi = 0.5, theta = 0.5)
}
