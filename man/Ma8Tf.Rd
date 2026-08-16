% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ma8Tf}
\alias{Ma8Tf}
\title{Eq (3.110): seven zeros spaced evenly round the unit circle, at every}
\usage{
Ma8Tf(z)
}
\arguments{
\item{z}{See Usage.}
}
\description{
multiple of fs/8 except DC.  For fs = 1000 Hz the book puts them at 125, 250, 375 and 500 Hz.
}
\value{
A list with \code{H}, \code{z}, \code{n_taps}, \code{n_zeros}, \code{zeros_at_multiples_of_fs_over_8}, \code{dc_gain}, \code{always_stable}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Ma8Tf(V)
}
