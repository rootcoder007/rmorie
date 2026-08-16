% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SeizCohere}
\alias{SeizCohere}
\title{SeizCohere}
\usage{
SeizCohere(channels, fs, window, step = NULL, bands = NULL, nperseg = NULL)
}
\arguments{
\item{channels}{See Usage.}
\item{fs}{See Usage.}
\item{window}{See Usage.}
\item{step}{Defaults to \code{NULL}.}
\item{bands}{Defaults to \code{NULL}.}
\item{nperseg}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_corr implementation; see the file header for the source it follows.
}
\value{
A list with \code{times}, \code{coherence}, \code{bands}, \code{window}, \code{step}, \code{nperseg}, \code{n_windows}, \code{n_channels}, \code{sustained_criterion}, \code{method}.
}
\keyword{internal}
