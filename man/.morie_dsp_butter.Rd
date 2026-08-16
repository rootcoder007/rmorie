% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dsp_butter}
\alias{.morie_dsp_butter}
\title{Internal helper: Butterworth digital filter design}
\usage{
.morie_dsp_butter(n, W, type = c("low", "high", "pass", "stop"))
}
\description{
`W` is the normalized cutoff (Nyquist = 1), scalar for low/high-pass, length-2 for band-pass. Returns list(b, a) matching signal::butter\'s convention.
}
\value{
A list with \code{b}, \code{a}.
}
\keyword{internal}
