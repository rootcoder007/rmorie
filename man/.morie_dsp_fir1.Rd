% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dsp_fir1}
\alias{.morie_dsp_fir1}
\title{Internal helper: windowed-sinc FIR design (signal::fir1 convention)}
\usage{
.morie_dsp_fir1(n, W, type = c("low", "high", "pass"), window = "hamming")
}
\description{
`n` is the filter ORDER (returns n + 1 taps); `W` normalized with Nyquist = 1.
}
\value{
A vector.
}
\keyword{internal}
