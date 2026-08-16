% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SwtDen}
\alias{SwtDen}
\title{SWT (cycle-spinning) denoising, eqs (8.103)-(8.104) + Coifman &}
\usage{
SwtDen(x, wavelet = "db4", levels = 3, threshold = NULL, threshold_type = "soft")
}
\arguments{
\item{x}{See Usage.}
\item{wavelet}{Defaults to \code{"db4"}.}
\item{levels}{Defaults to \code{3}.}
\item{threshold}{Defaults to \code{NULL}.}
\item{threshold_type}{Defaults to \code{"soft"}.}
}
\description{
Donoho Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{denoised}, \code{threshold}, \code{sigma}, \code{n_zeroed}, \code{n_coeffs}, \code{n_shifts}, \code{residual_energy}, \code{levels}, \code{wavelet}, \code{threshold_type}, \code{method}.
}
\keyword{internal}
