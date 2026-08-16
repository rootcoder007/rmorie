% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WtEntropy}
\alias{WtEntropy}
\title{Wavelet (relative-energy Shannon) entropy, Rosso et al. (2001)}
\usage{
WtEntropy(x, wavelet = "db4", levels = 3, base = "e")
}
\arguments{
\item{x}{See Usage.}
\item{wavelet}{Defaults to \code{"db4"}.}
\item{levels}{Defaults to \code{3}.}
\item{base}{Defaults to \code{"e"}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{entropy}, \code{max_entropy}, \code{normalized_entropy}, \code{relative_energy}, \code{labels}, \code{levels}, \code{base}, \code{wavelet}, \code{method}.
}
\keyword{internal}
