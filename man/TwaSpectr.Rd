% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TwaSpectr}
\alias{TwaSpectr}
\title{Section 9.10 citing Smith et al., Circulation 77(1):110-121, 1988}
\usage{
TwaSpectr(twaves, noiselo = 0.33, noisehi = 0.45)
}
\arguments{
\item{twaves}{See Usage.}
\item{noiselo}{Defaults to \code{0.33}.}
\item{noisehi}{Defaults to \code{0.45}.}
}
\description{
Alternation every other beat is a period of exactly two beats, so it lands on 0.5 cycles per beat -- the last bin of the beat-series spectrum. An EVEN beat count is required or that is not an exact bin.
}
\value{
A list with \code{valt}, \code{kscore}, \code{altpower}, \code{noisemean}, \code{noisesd}, \code{nbeats}, \code{npoints}, \code{cyclesperbeat}, \code{present}, \code{method}.
}
\keyword{internal}
