% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{find_homologous_segments}
\alias{find_homologous_segments}
\title{find_homologous_segments}
\usage{
find_homologous_segments(group1, group2, scoring, weights1 = NULL, weights2 = NULL, seq_type = "aa", window = 30L, n_peaks = 20L, threshold = 0.7, max_len = 150L, corr_method = "fft")
}
\arguments{
\item{group1}{See Usage.}
\item{group2}{See Usage.}
\item{scoring}{See Usage.}
\item{weights1}{Defaults to \code{NULL}.}
\item{weights2}{Defaults to \code{NULL}.}
\item{seq_type}{Defaults to \code{"aa"}.}
\item{window}{Defaults to \code{30L}.}
\item{n_peaks}{Defaults to \code{20L}.}
\item{threshold}{Defaults to \code{0.7}.}
\item{max_len}{Defaults to \code{150L}.}
\item{corr_method}{Defaults to \code{"fft"}.}
}
\description{
Part of the mafft_native implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
