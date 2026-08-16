% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WvDist}
\alias{WvDist}
\title{Wigner-Ville distribution, eq (8.123)}
\usage{
WvDist(x, fs = 1, nfreq = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{nfreq}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{tfd}, \code{times}, \code{freqs}, \code{peak_freq}, \code{total_energy}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
WvDist(V)
}
