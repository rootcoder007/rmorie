% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_xcorr_direct}
\alias{.mafft_xcorr_direct}
\title{.mafft_xcorr_direct}
\usage{
.mafft_xcorr_direct(a, b, size)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{b}{A vector; its length is taken and its elements indexed.}
\item{size}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the mafft_native implementation. Called by \code{correlation}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.mafft_xcorr_direct(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, size = 5L)
}
