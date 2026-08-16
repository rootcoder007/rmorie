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
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{sigma}{Defaults to \code{1}.}
\item{nfreq}{Defaults to \code{NULL}.}
\item{maxlag}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{tfd}, \code{times}, \code{freqs}, \code{sigma}, \code{maxlag}, \code{peak_freq}, \code{crossterm_ratio}, \code{method}.
}
\keyword{internal}
