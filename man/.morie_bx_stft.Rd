% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_stft}
\alias{.morie_bx_stft}
\title{.morie_bx_stft}
\usage{
.morie_bx_stft(x, nwin, hop)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{nwin}{A count; the body uses it as \code{seq_len(...)}.}
\item{hop}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{FecgNmf}. See the file header for the source the module follows.
}
\value{
A list with \code{re}, \code{im}, \code{mag}, \code{win}.
}
\keyword{internal}
\examples{
set.seed(4)
x <- sin(2 * pi * (1:128) / 16)
.morie_bx_stft(x, nwin = 32L, hop = 16L)
}
