% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsalpcspec}
\alias{.bsalpcspec}
\title{Magnitude-squared response of the all-pole LPC filter, 0..fs/2 Hz}
\usage{
.bsalpcspec(a, fs, npts = 1024L)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{npts}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1024L}.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{CadAcou}, \code{InfantCry}, \code{SpeechFeat} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{freqs}, \code{power}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bsalpcspec(V, V)
}
