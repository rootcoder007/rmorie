% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dsp_findpeaks}
\alias{.morie_dsp_findpeaks}
\title{Internal helper: local maxima above a height, minimum spacing}
\usage{
.morie_dsp_findpeaks(x, min_height = -Inf, min_distance = 1L)
}
\description{
Internal helper: local maxima above a height, minimum spacing
}
\value{
The value of \code{sort}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_dsp_findpeaks(V)
}
