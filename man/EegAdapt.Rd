% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EegAdapt}
\alias{EegAdapt}
\title{EegAdapt}
\usage{
EegAdapt(x, fs, window = NULL, step = NULL, order = 4, threshold = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{window}{Defaults to \code{NULL}.}
\item{step}{Defaults to \code{NULL}.}
\item{order}{Defaults to \code{4}.}
\item{threshold}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_ccf implementation; see the file header for the source it follows.
}
\value{
A list with \code{d}, \code{d_fixed_reference}, \code{times}, \code{boundaries}, \code{n_boundaries}, \code{threshold}, \code{median}, \code{mad}, \code{window}, \code{step}, \code{order}, \code{fs}, \code{reference_restarts_at_boundaries}, \code{robust_threshold}, \code{method}.
}
\keyword{internal}
