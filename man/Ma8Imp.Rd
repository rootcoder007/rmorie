% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ma8Imp}
\alias{Ma8Imp}
\title{Eq (3.109): eight equal taps.  Equal weighting is why the stopband}
\usage{
Ma8Imp(n = NULL)
}
\arguments{
\item{n}{Defaults to \code{NULL}.}
}
\description{
attenuation is poor -- the book notes no more than about -20 dB at most frequencies, so a filtered noisy ECG is still visibly noisy.
}
\value{
A list with \code{h}, \code{value}, \code{index}, \code{n_taps}, \code{sum}, \code{finite}, \code{equal_weights}, \code{attenuation_is_poor}, \code{method}.
}
\keyword{internal}
\examples{
Ma8Imp()
}
