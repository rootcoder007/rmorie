% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FtConv}
\alias{FtConv}
\title{Eqs (4.61)-(4.62): the Fourier transform turns the convolution into a}
\usage{
FtConv(x, h, omega, dt = 1)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{dt}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
product, which eq (4.63) then turns into a sum.  The convolution is scaled by dt as in eq (3.30), so the identity holds in the continuous-time sense rather than up to a sampling-interval factor.
}
\value{
A list with \code{y}, \code{Y}, \code{X}, \code{H}, \code{XH}, \code{max_difference}, \code{holds}, \code{method}.
}
\keyword{internal}
\examples{
FtConv(x = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5, omega = c(1, 2, 3, 4, 5, 6, 7, 8))
}
