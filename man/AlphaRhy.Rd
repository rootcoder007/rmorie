% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{AlphaRhy}
\alias{AlphaRhy}
\title{The alpha band by convention; the SAME test serves the other bands,}
\usage{
AlphaRhy(x, fs, band = c(8, 13), threshold = 0.3)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{band}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{c(8, 13)}.}
\item{threshold}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.3}.}
}
\description{
so the band is an argument, not a constant.  Both the band AND the amplitude have to be satisfied -- a lag in band with a tiny peak is not a rhythm.
}
\value{
A list with \code{acf}, \code{band}, \code{lag_range}, \code{peak_lag}, \code{peak}, \code{frequency_hz}, \code{threshold}, \code{present}, \code{needs_both_the_band_and_the_amplitude}, \code{same_test_serves_other_bands}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
AlphaRhy(x = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
