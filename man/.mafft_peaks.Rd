% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_peaks}
\alias{.mafft_peaks}
\title{.mafft_peaks}
\usage{
.mafft_peaks(lags, c, n_peaks)
}
\arguments{
\item{lags}{A vector; its length is taken and its elements indexed.}
\item{c}{A vector; its length is taken.}
\item{n_peaks}{See Usage.}
}
\description{
A step of the mafft_native implementation. Called by \code{find_homologous_segments}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
