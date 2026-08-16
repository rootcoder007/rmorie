% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_fft_size}
\alias{.mafft_fft_size}
\title{.mafft_fft_size}
\usage{
.mafft_fft_size(n, m)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{m}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the mafft_native implementation. Called by \code{.mafft_xcorr_fft}, \code{correlation}. See the file header for the source the module follows.
}
\value{
The value of \code{size}, as built in the body.
}
\keyword{internal}
\examples{
.mafft_fft_size(n = 5L, m = 5L)
}
