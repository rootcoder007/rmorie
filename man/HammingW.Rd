% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HammingW}
\alias{HammingW}
\title{W(n) = 0.54 - 0.46 cos(2 pi n/(N-1)).  The 0.54/0.46 split cancels}
\usage{
HammingW(N)
}
\arguments{
\item{N}{See Usage.}
}
\description{
the rectangle\'s largest sidelobe, about -43 dB, at the cost of a wider main lobe than the Hann.  It does NOT reach zero at the ends -- w(0) = 0.08 -- which matters when windows are overlapped and added.
}
\value{
A list with \code{w}, \code{N}, \code{sum}, \code{endpoints}, \code{reaches_zero_at_the_ends}, \code{coherent_gain}, \code{symmetric}, \code{method}.
}
\keyword{internal}
\examples{
HammingW(N = 5L)
}
