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
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{nperseg}{Defaults to \code{64}.}
\item{noverlap}{Defaults to \code{NULL}.}
\item{window}{Defaults to \code{"hann"}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{spectrogram}, \code{stft}, \code{times}, \code{freqs}, \code{nperseg}, \code{hop}, \code{window}, \code{n_frames}, \code{total_energy}, \code{peak_freq}, \code{method}.
}
\keyword{internal}
