% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ErdErs}
\alias{ErdErs}
\title{ErdErs: event-related desynchronisation / synchronisation for BCI}
\usage{
ErdErs(eeg, fs, ref_window, active_window, band = c(8, 13))
}
\arguments{
\item{eeg}{See Usage.}
\item{fs}{See Usage.}
\item{ref_window}{See Usage.}
\item{active_window}{See Usage.}
\item{band}{Defaults to \code{c(8, 13)}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{erd_percent}, \code{ref_power}, \code{active_power}, \code{power_ratio}, \code{event}, \code{band_hz}, \code{ref_window_s}, \code{active_window_s}, \code{ref_samples}, \code{active_samples}, \code{fs_hz}, \code{sign_convention}, \code{units}, \code{method}.
}
\keyword{internal}
