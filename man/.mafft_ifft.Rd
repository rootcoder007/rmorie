% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_ifft}
\alias{.mafft_ifft}
\title{.mafft_ifft}
\usage{
.mafft_ifft(x)
}
\arguments{
\item{x}{A vector; its length is taken.}
}
\description{
A step of the mafft_native implementation. Called by \code{.mafft_xcorr_fft}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mafft_ifft(V)
}
