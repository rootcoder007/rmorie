% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannW}
\alias{HannW}
\title{W(n) = 0.5[1 - cos(2 pi n/(N-1))].  Reaches exactly zero at both}
\usage{
HannW(N)
}
\arguments{
\item{N}{See Usage.}
}
\description{
ends, so overlapped Hann windows add to a constant at 50 per cent overlap -- the property that makes it the default for overlap-add analysis. Not to be confused with the Hann FILTER of eq (3.100), a three-tap 1:2:1 smoother; this is a taper applied to a data segment.
}
\value{
A list with \code{w}, \code{N}, \code{sum}, \code{endpoints}, \code{reaches_zero_at_the_ends}, \code{coherent_gain}, \code{not_the_hann_filter_of_eq_3_100}, \code{symmetric}, \code{method}.
}
\keyword{internal}
\examples{
HannW(N = 5L)
}
