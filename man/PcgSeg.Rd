% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcgSeg}
\alias{PcgSeg}
\title{PcgSeg}
\usage{
PcgSeg(x, fs, window = NULL, step = NULL, order = 6, threshold = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{window}{Defaults to \code{NULL}.}
\item{step}{Defaults to \code{NULL}.}
\item{order}{Defaults to \code{6}.}
\item{threshold}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{sem}, \code{sem_fixed_reference}, \code{times}, \code{boundaries}, \code{n_boundaries}, \code{threshold}, \code{median}, \code{mad}, \code{window}, \code{step}, \code{order}, \code{fs}, \code{reference_restarted_at_boundaries}, \code{robust_threshold}, \code{method}.
}
\keyword{internal}
