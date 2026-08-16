% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SparseVector}
\alias{SparseVector}
\title{SparseVector}
\usage{
SparseVector(queries, threshold, c = 1L, epsilon = 1, threshold_noise = 0, query_noise = NULL)
}
\arguments{
\item{queries}{See Usage.}
\item{threshold}{See Usage.}
\item{c}{Defaults to \code{1L}.}
\item{epsilon}{Defaults to \code{1}.}
\item{threshold_noise}{Defaults to \code{0}.}
\item{query_noise}{Defaults to \code{NULL}.}
}
\description{
Part of the sp_fill implementation; see the file header for the source it follows.
}
\value{
A list with \code{above}, \code{released}, \code{halted_at}, \code{n_above}, \code{noisy_threshold}, \code{noise_scales}, \code{epsilon_split}, \code{epsilon}, \code{c}, \code{cost_scales_with_c_not_with_m}, \code{answered}, \code{n}, \code{method}.
}
\keyword{internal}
