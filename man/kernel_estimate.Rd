% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{kernel_estimate}
\alias{kernel_estimate}
\title{kernel_estimate}
\usage{
kernel_estimate(x, y, omegas, kind = "positive") 
}
\arguments{
\item{x}{Passed to \code{rbind}.}
\item{y}{Passed to \code{rbind}.}
\item{omegas}{Passed to \code{favor_features}.}
\item{kind}{Passed to \code{favor_features}. Defaults to \code{"positive"}.}
}
\description{
A step of the perfat_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.dot}.
}
\keyword{internal}
\examples{
kernel_estimate(x = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), y = c(1, 2, 3, 4, 5, 6, 7, 8), omegas = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
