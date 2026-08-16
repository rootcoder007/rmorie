% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HrvTv}
\alias{HrvTv}
\title{Time-varying HRV band powers, Sec 8.12}
\usage{
HrvTv(rr_intervals, fs_resamp = 4, window_len = 64, noverlap = NULL, standard = "taskforce")
}
\arguments{
\item{rr_intervals}{See Usage.}
\item{fs_resamp}{Defaults to \code{4}.}
\item{window_len}{Defaults to \code{64}.}
\item{noverlap}{Defaults to \code{NULL}.}
\item{standard}{Defaults to \code{"taskforce"}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{times}, \code{vlf}, \code{lf}, \code{hf}, \code{total_power}, \code{lf_hf_ratio}, \code{lf_percent}, \code{hf_percent}, \code{bands}, \code{standard}, \code{mean_rr}, \code{mean_hr}, \code{resampled}, \code{fs_resamp}, \code{method}.
}
\keyword{internal}
