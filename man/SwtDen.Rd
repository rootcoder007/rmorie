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
\item{x}{Passed to \code{.tf_need}.}
\item{wavelet}{Passed to \code{.tf_dwt}. Defaults to \code{"db4"}.}
\item{levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
\item{threshold}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{threshold_type}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"soft"}.}
}
\description{
Donoho Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{denoised}, \code{threshold}, \code{sigma}, \code{n_zeroed}, \code{n_coeffs}, \code{n_shifts}, \code{residual_energy}, \code{levels}, \code{wavelet}, \code{threshold_type}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(5)
x <- sin(2 * pi * (1:128) / 16) + rnorm(128, 0, 0.3)
str(SwtDen(x, wavelet = "db4", levels = 3), max.level = 1)
}
