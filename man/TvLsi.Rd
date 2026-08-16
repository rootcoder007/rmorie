% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TvLsi}
\alias{TvLsi}
\title{A time-variant system needs h(n, m), one response per output instant:}
\usage{
TvLsi(x, h)
}
\arguments{
\item{x}{See Usage.}
\item{h}{See Usage.}
}
\description{
y(n) = sum_m h(n, m) x(n - m). The convolution sum still holds instant by instant, but the kernel is no longer shift-invariant, so no single transfer function describes it -- the contrast with eq (3.36) is the point.
}
\value{
A list with \code{y}, \code{n}, \code{kernel_lengths}, \code{shift_invariant}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
TvLsi(V, V)
}
