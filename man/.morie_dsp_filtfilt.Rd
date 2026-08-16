% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dsp_filtfilt}
\alias{.morie_dsp_filtfilt}
\title{Internal helper: zero-phase forward-backward filtering}
\usage{
.morie_dsp_filtfilt(b, a, x)
}
\description{
Odd-extension padding of length 3 * (filter order), forward pass, reversed pass, trim (the MATLAB/scipy filtfilt convention).
}
\value{
The value of \code{y}, as built in the body.
}
\keyword{internal}
\examples{
.morie_dsp_filtfilt(b = 5L, a = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
