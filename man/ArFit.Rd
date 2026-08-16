% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ArFit}
\alias{ArFit}
\title{Section 7.5: S(f) = G^2 / |A(exp(-j 2 pi f / fs))|^2, a smooth}
\usage{
ArFit(x, order, fs = 1, nfreq = 256)
}
\arguments{
\item{x}{See Usage.}
\item{order}{See Usage.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{nfreq}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{256}.}
}
\description{
spectrum from P+1 parameters.  That smoothness is also the trap: the model can only make P/2 peaks, so extra resonances merge silently.
}
\value{
The value of \code{fit}, as built in the body.
}
\keyword{internal}
\examples{
ArFit(x = c(1, 2, 3, 4, 5, 6, 7, 8), order = 5L)
}
