% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BilinWarp}
\alias{BilinWarp}
\title{Prewarp a digital cutoff before handing it to an analog design}
\usage{
BilinWarp(omega, T = 1)
}
\arguments{
\item{omega}{A digital frequency or vector of them, strictly inside   minus pi to pi.}
\item{T}{The sampling interval, which must be positive.}
}
\description{
Equation (3.141). The mapping is nonlinear, so a digital cutoff cannot be given to an analog design unchanged: skipping this step puts the realised cutoff BELOW the one asked for, and increasingly so towards Nyquist. A digital frequency of pi maps to an infinite analog frequency and is refused rather than clipped.
}
\value{
A list with the analog frequency \code{Omega} and flags   recording that the map is nonlinear, that prewarping is required and   that the compression is severe near Nyquist.
}
\keyword{internal}
\examples{
BilinWarp(omega = list(a = 1, b = 2))
}
