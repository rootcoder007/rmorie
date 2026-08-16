% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_xcorr_fft}
\alias{.mafft_xcorr_fft}
\title{.mafft_xcorr_fft}
\usage{
.mafft_xcorr_fft(a, b)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{A vector; its length is taken.}
}
\description{
A step of the mafft_native implementation. Called by \code{correlation}. See the file header for the source the module follows.
}
\value{
A list with \code{out}, \code{size}.
}
\keyword{internal}
