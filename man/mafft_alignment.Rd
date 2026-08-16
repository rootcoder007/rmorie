% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mafft_alignment}
\alias{mafft_alignment}
\title{mafft_alignment}
\usage{
mafft_alignment(sequences, method = "FFT-NS-2", seq_type = NULL, raw_matrix = NULL, freqs = NULL, s_a = 0.06, s_op = 2.4, matrix = "normalized", window = 30L, n_peaks = 20L, threshold = 0.7, max_len = 150L, max_iterate = 16L)
}
\arguments{
\item{sequences}{Passed to \code{.mafft_clean}.}
\item{method}{One of \code{"FFT-NS-2"}, \code{"FFT-NS-i"}, \code{"NW-NS-2"}. Defaults to \code{"FFT-NS-2"}.}
\item{seq_type}{Passed to \code{.mafft_clean}.}
\item{raw_matrix}{Defaults to \code{NULL}.}
\item{freqs}{Defaults to \code{NULL}.}
\item{s_a}{Defaults to \code{0.06}.}
\item{s_op}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2.4}.}
\item{matrix}{Carried through into a list the body builds. Defaults to \code{"normalized"}.}
\item{window}{Carried through into a list the body builds. Defaults to \code{30L}.}
\item{n_peaks}{Carried through into a list the body builds. Defaults to \code{20L}.}
\item{threshold}{Carried through into a list the body builds. Defaults to \code{0.7}.}
\item{max_len}{Carried through into a list the body builds. Defaults to \code{150L}.}
\item{max_iterate}{Defaults to \code{16L}.}
}
\description{
A step of the mafft_native implementation. Called by \code{morie_mafft}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{alignment}, \code{score}, \code{method}, \code{seq_type}, \code{length}, \code{n}, \code{s_a}, \code{s_op}, \code{matrix_mode}, \code{tree}, \code{refine_rounds}, \code{note}.
}
\keyword{internal}
