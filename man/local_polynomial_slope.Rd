% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{local_polynomial_slope}
\alias{local_polynomial_slope}
\title{local_polynomial_slope}
\usage{
local_polynomial_slope(v, y, kink, bandwidth, order = 2L, side = "right", kernel = "triangular")
}
\arguments{
\item{v}{Passed to \code{.side_fit}.}
\item{y}{Passed to \code{.side_fit}.}
\item{kink}{Passed to \code{.side_fit}.}
\item{bandwidth}{Passed to \code{.side_fit}.}
\item{order}{Defaults to \code{2L}.}
\item{side}{One of \code{"left"}, \code{"right"}. Defaults to \code{"right"}.}
\item{kernel}{One of \code{"triangular"}, \code{"uniform"}. Defaults to \code{"triangular"}.}
}
\description{
A step of the bnskt2_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.side_fit}.
}
\keyword{internal}
