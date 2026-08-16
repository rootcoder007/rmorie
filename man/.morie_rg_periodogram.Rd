% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_periodogram}
\alias{.morie_rg_periodogram}
\title{.morie_rg_periodogram}
\usage{
.morie_rg_periodogram(xs, fs)
}
\arguments{
\item{xs}{A vector; its length is taken.}
\item{fs}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_stat implementation. Called by \code{EmgFreq}, \code{FdVag}, \code{SigFeatures}. See the file header for the source the module follows.
}
\value{
A list with \code{psd}, \code{freqs}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_periodogram(V, V)
}
