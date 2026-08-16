% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Gtfd}
\alias{Gtfd}
\title{Cohen\'s class generalised TFD, eqs (8.124)-(8.127)}
\usage{
Gtfd(x, fs = 1, kernel = "spwvd", nfreq = NULL, tsmooth = NULL, fsmooth = NULL)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{kernel}{Defaults to \code{"spwvd"}.}
\item{nfreq}{Defaults to \code{NULL}.}
\item{tsmooth}{Defaults to \code{NULL}.}
\item{fsmooth}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{tfd}, \code{times}, \code{freqs}, \code{kernel}, \code{tsmooth}, \code{fsmooth}, \code{peak_freq}, \code{crossterm_ratio}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Gtfd(V)
}
