% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsapsd}
\alias{.bsapsd}
\title{Periodogram of x sampled at fs Hz with a Hann window, 0 .. fs/2}
\usage{
.bsapsd(x, fs, detrend = TRUE)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{detrend}{Defaults to \code{TRUE}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{freqs}, \code{power}.
}
\keyword{internal}
