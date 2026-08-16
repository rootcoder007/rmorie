% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VocalTract}
\alias{VocalTract}
\title{VocalTract}
\usage{
VocalTract(y, fs, pitch_period = NULL, cutoff = NULL, pitch_range = c(0.002, 0.02))
}
\arguments{
\item{y}{See Usage.}
\item{fs}{See Usage.}
\item{pitch_period}{Defaults to \code{NULL}.}
\item{cutoff}{Defaults to \code{NULL}.}
\item{pitch_range}{Defaults to \code{c(0.002, 0.02)}.}
}
\description{
Part of the rangayyan_cep implementation; see the file header for the source it follows.
}
\value{
A list with \code{response}, \code{cepstrum}, \code{cutoff}, \code{pitch_period}, \code{pitch_hz}, \code{peak_quefrency}, \code{fs}, \code{n}, \code{method}.
}
\keyword{internal}
