% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TwidCS}
\alias{TwidCS}
\title{Eq (3.84): W_N^(nk) = cos(.) - j sin(.).  Note the MINUS on the sine:}
\usage{
TwidCS(npoints, n, k)
}
\arguments{
\item{npoints}{See Usage.}
\item{n}{See Usage.}
\item{k}{See Usage.}
}
\description{
the DFT projects onto the conjugated exponential, and that sign is the commonest transcription error in a hand-written DFT.
}
\value{
A list with \code{W}, \code{cos}, \code{sin}, \code{angle}, \code{N}, \code{n}, \code{k}, \code{method}.
}
\keyword{internal}
\examples{
TwidCS(npoints = 5L, n = 5L, k = 5L)
}
