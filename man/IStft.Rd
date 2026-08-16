% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IStft}
\alias{IStft}
\title{Weighted overlap-add inverse STFT, Griffin & Lim (1984) eq (6)}
\usage{
IStft(stft, window = "hann", hop = NULL)
}
\arguments{
\item{stft}{See Usage.}
\item{window}{Defaults to \code{"hann"}.}
\item{hop}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{signal}, \code{n}, \code{hop}, \code{valid_start}, \code{valid_end}, \code{n_frames}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
IStft(M)
}
