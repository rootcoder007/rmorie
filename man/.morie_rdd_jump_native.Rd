% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rdd_jump_native}
\alias{.morie_rdd_jump_native}
\title{Internal helper: two-sided sharp RD estimate at the cutoff}
\usage{
.morie_rdd_jump_native(x, y, cutoff, h, p = 1, kernel = "triangular", deriv = 0L)
}
\description{
`deriv = 0` gives the level jump (sharp RD), `deriv = 1` the slope jump (regression kink). Returns estimate, NN-robust SE, and side fits.
}
\value{
A list with \code{estimate}, \code{se}, \code{n}, \code{left}, \code{right}.
}
\keyword{internal}
