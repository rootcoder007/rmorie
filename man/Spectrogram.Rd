% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Spectrogram}
\alias{Spectrogram}
\title{STFT spectrogram, eq (8.8); |STFT|^2 per the text after eq (8.9)}
\usage{
Spectrogram(x, fs = 1, nperseg = 64, noverlap = NULL, window = "hann")
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{nperseg}{Defaults to \code{64}.}
\item{noverlap}{Defaults to \code{NULL}.}
\item{window}{Passed to \code{.tf_win}. Defaults to \code{"hann"}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{HrvTv}. See the file header for the source the module follows.
}
\value{
A list with \code{spectrogram}, \code{stft}, \code{times}, \code{freqs}, \code{nperseg}, \code{hop}, \code{window}, \code{n_frames}, \code{total_energy}, \code{peak_freq}, \code{method}.
}
\keyword{internal}
