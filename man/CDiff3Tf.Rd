% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CDiff3Tf}
\alias{CDiff3Tf}
\title{Eq (3.129).  The factored form is the point: the operator IS a}
\usage{
CDiff3Tf(z, T = 1)
}
\arguments{
\item{z}{Coerced to complex by the body, with \code{as.complex}.}
\item{T}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
first-order difference in series with a two-point moving average, so it may be built as that cascade.  Zeros at z = 1 and z = -1 make it a bandpass, and the one at Nyquist is what kills the noise boost.
}
\value{
A list with \code{H}, \code{z}, \code{T}, \code{cascade}, \code{max_difference}, \code{cascade_agrees}, \code{zeros}, \code{bandpass}, \code{is_first_difference_times_two_point_ma}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CDiff3Tf(V)
}
