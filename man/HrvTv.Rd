% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HrvTv}
\alias{HrvTv}
\title{Time-varying HRV band powers, Sec 8.12}
\usage{
HrvTv(rr_intervals, fs_resamp = 4, window_len = 64, noverlap = NULL, 
    standard = "taskforce") 
}
\arguments{
\item{rr_intervals}{Passed to \code{.tf_need}.}
\item{fs_resamp}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{4}.}
\item{window_len}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{64}.}
\item{noverlap}{Passed to \code{Spectrogram}.}
\item{standard}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"taskforce"}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{times}, \code{vlf}, \code{lf}, \code{hf}, \code{total_power}, \code{lf_hf_ratio}, \code{lf_percent}, \code{hf_percent}, \code{bands}, \code{standard}, \code{mean_rr}, \code{mean_hr}, \code{resampled}, \code{fs_resamp}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HrvTv(V)
}
