% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsLPassTf}
\alias{QrsLPassTf}
\title{Eq (4.7): H(z) = (1/32)(1 - z^-6)^2 / (1 - z^-1)^2, evaluated in its}
\usage{
QrsLPassTf(freq, fs = 200)
}
\arguments{
\item{freq}{See Usage.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{200}.}
}
\description{
equivalent finite form (sum_{k=0}^{5} z^-k)^2 / 32 so the apparent pole at z = 1 is removed exactly rather than numerically.  fc = 11 Hz, 5-sample delay, >35 dB at 60 Hz -- all tied to fs = 200 Hz.
}
\value{
A list with \code{freq}, \code{mag}, \code{phase}, \code{b}, \code{a}, \code{fs}, \code{fsnote}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsLPassTf(V)
}
