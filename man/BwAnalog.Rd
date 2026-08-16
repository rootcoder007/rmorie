% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwAnalog}
\alias{BwAnalog}
\title{The analog Butterworth transfer function from its left-half-plane poles}
\usage{
BwAnalog(Omega_c, N, G = NULL, s = NULL)
}
\arguments{
\item{Omega_c}{The cutoff, which must be positive.}
\item{N}{The filter order, at least one.}
\item{G}{The overall gain, or NULL to normalise the gain at direct   current to one.}
\item{s}{Optional points at which to evaluate the transfer function; a   pole is refused rather than returned as an infinity.}
}
\description{
Equation (3.138), built from the N left-half-plane poles ONLY -- the right-half-plane poles of the magnitude-squared function belong to the unstable mirror and taking them would produce a filter that blows up. With no gain supplied the constant normalises the direct-current gain to one, which makes it the cutoff raised to the order. The denominator coefficients are real to rounding because the poles arrive in conjugate pairs; the largest imaginary residue is returned so that claim can be checked rather than trusted.
}
\value{
A list with the poles, the real denominator coefficients, the   gain, the transfer function where it was asked for, and the   imaginary residue that shows the coefficients really are real.
}
\keyword{internal}
