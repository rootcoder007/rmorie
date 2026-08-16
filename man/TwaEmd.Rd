% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TwaEmd}
\alias{TwaEmd}
\title{Odd/even T-wave alternans after EMD detrending, Sec 9.2.3 + Sec 9.4}
\usage{
TwaEmd(ecg, fs = 250, r_peaks = NULL, twa_window = c(0.15, 0.4), max_imfs = 6)
}
\arguments{
\item{ecg}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{250}.}
\item{r_peaks}{Defaults to \code{NULL}.}
\item{twa_window}{A vector; indexed elementwise. Defaults to \code{c(0.15, 0.4)}.}
\item{max_imfs}{Defaults to \code{6}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{twa_amplitude}, \code{twa_rms}, \code{odd_mean}, \code{even_mean}, \code{difference}, \code{n_beats}, \code{n_odd}, \code{n_even}, \code{r_peaks}, \code{rpeaks_supplied}, \code{method}.
}
\keyword{internal}
