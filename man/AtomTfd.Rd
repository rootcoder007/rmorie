% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{AtomTfd}
\alias{AtomTfd}
\title{Matching-pursuit TFD, eq (9.15) over the Gabor dictionary}
\usage{
AtomTfd(x, fs = 1, dictionary = "gabor", max_atoms = 8, nfreq = NULL, min_decay = 0.001)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{dictionary}{Defaults to \code{"gabor"}.}
\item{max_atoms}{Defaults to \code{8}.}
\item{nfreq}{Defaults to \code{NULL}.}
\item{min_decay}{Defaults to \code{0.001}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{tfd}, \code{times}, \code{freqs}, \code{atoms}, \code{n_atoms}, \code{decay}, \code{residual_energy}, \code{explained}, \code{peak_freq}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
AtomTfd(V)
}
