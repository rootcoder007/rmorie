% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BiorDwt}
\alias{BiorDwt}
\title{Biorthogonal 5/3 (CDF) transform via lifting}
\usage{
BiorDwt(x, wavelet = "bior2.2", levels = 3) 
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{wavelet}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"bior2.2"}.}
\item{levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{approx}, \code{details}, \code{coeffs}, \code{lengths}, \code{levels}, \code{reconstructed}, \code{max_reconstruction_error}, \code{symmetric}, \code{wavelet}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
BiorDwt(V)
}
