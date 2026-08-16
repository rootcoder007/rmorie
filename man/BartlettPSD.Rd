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
\item{x}{See Usage.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{n_segments}{Defaults to \code{NULL}.}
\item{segment_length}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_sig implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{psd}, \code{freqs}, \code{n_segments}, \code{segment_length}, \code{method}.
}
\keyword{internal}
