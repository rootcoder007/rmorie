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
\item{twaves}{A vector; its length is taken.}
\item{noiselo}{The body requires: noise band must satisfy 0 < noiselo < noisehi < 0.5. Defaults to \code{0.33}.}
\item{noisehi}{The body requires: noise band must satisfy 0 < noiselo < noisehi < 0.5. Defaults to \code{0.45}.}
}
\description{
Alternation every other beat is a period of exactly two beats, so it lands on 0.5 cycles per beat -- the last bin of the beat-series spectrum. An EVEN beat count is required or that is not an exact bin.
}
\value{
A list with \code{valt}, \code{kscore}, \code{altpower}, \code{noisemean}, \code{noisesd}, \code{nbeats}, \code{npoints}, \code{cyclesperbeat}, \code{present}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(5)
base <- exp(-((1:40 - 20) / 8)^2)
tw <- lapply(1:16, function(k) base * (1 + 0.05 * (-1)^k) +
                                0.01 * rnorm(40))
r <- TwaSpectr(tw)
str(r, max.level = 1)
}
