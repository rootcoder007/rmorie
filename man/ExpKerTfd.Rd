% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ExpKerTfd}
\alias{ExpKerTfd}
\title{Exponential-kernel (Choi-Williams) TFD}
\usage{
ExpKerTfd(x, fs = 1, sigma = 1, nfreq = NULL, maxlag = NULL)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{sigma}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{nfreq}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{maxlag}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{tfd}, \code{times}, \code{freqs}, \code{sigma}, \code{maxlag}, \code{peak_freq}, \code{crossterm_ratio}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
ExpKerTfd(V)
}
