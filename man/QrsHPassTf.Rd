% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsHPassTf}
\alias{QrsHPassTf}
\title{Eq (4.11): Hhp(z) = z^-16 - (1/32) Hlp(z).  An allpass (a pure}
\usage{
QrsHPassTf(freq, fs = 200)
}
\arguments{
\item{freq}{See Usage.}
\item{fs}{Defaults to \code{200}.}
}
\description{
16-sample delay) minus a scaled lowpass, so it shares the running sum already computed.  fc = 5 Hz and 80 ms delay hold at fs = 200 Hz.
}
\value{
A list with \code{freq}, \code{mag}, \code{phase}, \code{fs}, \code{fsnote}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsHPassTf(V)
}
