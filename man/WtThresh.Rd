% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WtThresh}
\alias{WtThresh}
\title{Wavelet shrinkage, eqs (8.103)-(8.105) with the universal threshold}
\usage{
WtThresh(x, wavelet = "db4", levels = 3, threshold_type = "soft", threshold = NULL)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{wavelet}{Passed to \code{.tf_dwt}. Defaults to \code{"db4"}.}
\item{levels}{Defaults to \code{3}.}
\item{threshold_type}{Defaults to \code{"soft"}.}
\item{threshold}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{PpgWtDen}. See the file header for the source the module follows.
}
\value{
A list with \code{denoised}, \code{threshold}, \code{sigma}, \code{n_zeroed}, \code{n_coeffs}, \code{sparsity}, \code{noise_removed}, \code{threshold_type}, \code{wavelet}, \code{method}.
}
\keyword{internal}
