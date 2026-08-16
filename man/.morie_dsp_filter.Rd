% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dsp_filter}
\alias{.morie_dsp_filter}
\title{Internal helper: direct-form II transposed IIR/FIR filter}
\usage{
.morie_dsp_filter(b, a, x)
}
\description{
Internal helper: direct-form II transposed IIR/FIR filter
}
\value{
A vector.
}
\keyword{internal}
\examples{
.morie_dsp_filter(b = 5L, a = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
