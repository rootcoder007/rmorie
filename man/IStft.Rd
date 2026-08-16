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
\item{stft}{A matrix; indexed by row and column.}
\item{window}{Passed to \code{.tf_win}. Defaults to \code{"hann"}.}
\item{hop}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{signal}, \code{n}, \code{hop}, \code{valid_start}, \code{valid_end}, \code{n_frames}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
IStft(M)
}
