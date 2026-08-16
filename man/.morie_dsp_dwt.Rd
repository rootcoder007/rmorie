% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dsp_dwt}
\alias{.morie_dsp_dwt}
\title{Internal helper: multi-level DWT (pyramid), periodic boundary}
\usage{
.morie_dsp_dwt(x, filter = "la8", n_levels = 4L)
}
\description{
Returns list(W = list of detail vectors level 1..J, V = final smooth, filter, n_levels, n).
}
\value{
A list with \code{W}, \code{V}, \code{filter}, \code{n_levels}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_dsp_dwt(V)
}
