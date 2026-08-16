% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BartlettPSD}
\alias{BartlettPSD}
\title{BartlettPSD}
\usage{
BartlettPSD(x, fs = 1, n_segments = NULL, segment_length = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{n_segments}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{segment_length}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_sig implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{psd}, \code{freqs}, \code{n_segments}, \code{segment_length}, \code{method}.
}
\keyword{internal}
\examples{
n <- 100
fs <- 64
f0 <- 8
x <- sin(2 * pi * f0 * (0:(n - 1))/fs)
BartlettPSD(x, fs = fs, n_segments = 4)
}
