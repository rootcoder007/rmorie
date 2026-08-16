% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfAcf}
\alias{MfAcf}
\title{Filtering with h(t) = K x(t0 - t) makes the convolution equivalent to}
\usage{
MfAcf(x, gain = 1, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{gain}{Defaults to \code{1}.}
\item{dt}{Defaults to \code{1}.}
}
\description{
CORRELATION, so y(t) = K phi_x(t - t0): the output is a delayed copy of the reference\\'s ACF and peaks at t0 with K times its energy.
}
\value{
A list with \code{y}, \code{acf}, \code{lags}, \code{peak_index}, \code{peak_value}, \code{expected_peak}, \code{energy}, \code{max_difference}, \code{equals_acf}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MfAcf(V)
}
