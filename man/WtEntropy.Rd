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
\item{wavelet}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"db4"}.}
\item{levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
\item{base}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"e"}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{entropy}, \code{max_entropy}, \code{normalized_entropy}, \code{relative_energy}, \code{labels}, \code{levels}, \code{base}, \code{wavelet}, \code{method}.
}
\keyword{internal}
