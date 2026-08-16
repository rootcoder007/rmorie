% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MatchedFilt}
\alias{MatchedFilt}
\title{MatchedFilt}
\usage{
MatchedFilt(ref, x = NULL, noise_psd = NULL, freqs = NULL, t0 = NULL, gain = 1, dt = 1)
}
\arguments{
\item{ref}{See Usage.}
\item{x}{Defaults to \code{NULL}.}
\item{noise_psd}{Defaults to \code{NULL}.}
\item{freqs}{Defaults to \code{NULL}.}
\item{t0}{Defaults to \code{NULL}.}
\item{gain}{Defaults to \code{1}.}
\item{dt}{Defaults to \code{1}.}
}
\description{
Part of the rangayyan_corr implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MatchedFilt(V)
}
