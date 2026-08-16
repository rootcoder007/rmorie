% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SigFeatures}
\alias{SigFeatures}
\title{The descriptors Rangayyan uses across Chapters 3, 5 and 6, each}
\usage{
SigFeatures(x, fs = 1, threshold = 0)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{threshold}{Defaults to \code{0}.}
}
\description{
computed by the function that owns its definition so the vector cannot disagree with the individual measures.  threshold defaults to 0 (every direction change counts); pass the book\'s 100 microvolts for a real EMG record or the count is dominated by noise.
}
\value{
A list with \code{mean}, \code{sd}, \code{rms}, \code{zero_crossings}, \code{zcr}, \code{turns}, \code{activity}, \code{mobility}, \code{form_factor}, \code{spectral_centroid}, \code{spectral_bandwidth}, \code{spectral_entropy}, \code{n}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
SigFeatures(x = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5))
}
