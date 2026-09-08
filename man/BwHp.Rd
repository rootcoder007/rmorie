% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwHp}
\alias{BwHp}
\title{The lowpass poles are reused -- a Butterworth highpass has the same}
\usage{
BwHp(cutoff_hz, order = 4, fs = 1000, z = NULL) 
}
\arguments{
\item{cutoff_hz}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1000}.}
\item{z}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_polyz}.}
}
\description{
pole radius -- and the N zeros move from z = -1 to z = +1.  The gain is renormalized at NYQUIST, since a highpass has no DC gain to normalize against and dividing there would be division by zero.
}
\value{
A list with \code{b}, \code{a}, \code{gain}, \code{H}, \code{N}, \code{cutoff_hz}, \code{fs}, \code{order}, \code{kind}, \code{zeros_at_plus_one}, \code{dc_gain}, \code{nyquist_gain}, \code{prewarped}, \code{normalized_at_nyquist}, \code{method}.
}
\keyword{internal}
\examples{
BwHp(cutoff_hz = 5L)
}
