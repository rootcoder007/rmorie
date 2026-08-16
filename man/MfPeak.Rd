% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfPeak}
\alias{MfPeak}
\title{Eq (4.38): M_y = |y(t0)|, the numerator of the SNR everything else}
\usage{
MfPeak(X, H, freqs, t0)
}
\arguments{
\item{X}{See Usage.}
\item{H}{See Usage.}
\item{freqs}{See Usage.}
\item{t0}{See Usage.}
}
\description{
maximizes.  It is a MAGNITUDE, so the phase of X H at t0 is what the optimal filter of eq (4.48) is chosen to align.
}
\value{
A list with \code{my}, \code{y}, \code{t0}, \code{phase}, \code{method}.
}
\keyword{internal}
\examples{
MfPeak(X = c(1, 2, 3, 4, 5, 6, 7, 8), H = c(1, 2, 3, 4, 5, 6, 7, 8), freqs = c(1, 2, 3, 4, 5, 6, 7, 8), t0 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
