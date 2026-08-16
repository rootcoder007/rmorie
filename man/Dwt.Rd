% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dwt}
\alias{Dwt}
\title{Dyadic DWT via the decimated filter bank, eqs (8.111)-(8.113)}
\usage{
Dwt(x, wavelet = "db4", levels = 3)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{wavelet}{Passed to \code{.tf_dwt}. Defaults to \code{"db4"}.}
\item{levels}{Defaults to \code{3}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{approx}, \code{details}, \code{coeffs}, \code{lengths}, \code{levels}, \code{wavelet}, \code{energy}, \code{method}.
}
\keyword{internal}
