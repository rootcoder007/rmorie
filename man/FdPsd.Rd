% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FdPsd}
\alias{FdPsd}
\title{Eqs (6.50)-(6.52): an fBm signal has PSD ~ 1/f^beta, and for a 1-D}
\usage{
FdPsd(psd, freqs, fmin = NULL, fmax = NULL)
}
\arguments{
\item{psd}{See Usage.}
\item{freqs}{See Usage.}
\item{fmin}{Defaults to \code{NULL}.}
\item{fmax}{Defaults to \code{NULL}.}
}
\description{
signal H = (beta-1)/2, FD = (5-beta)/2.  beta is MINUS the slope of log10 P against log10 f.  The DC bin is dropped: log(0) is undefined and DC carries the mean, not the scaling.
}
\value{
A list with \code{fd}, \code{beta}, \code{hurst}, \code{slope}, \code{intercept}, \code{n_bins}, \code{r_squared}, \code{in_range}, \code{band}, \code{method}.
}
\keyword{internal}
