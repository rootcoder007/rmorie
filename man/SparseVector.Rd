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
\item{queries}{Passed to \code{.morie_spx_chkv}.}
\item{threshold}{See Usage.}
\item{c}{Defaults to \code{1L}.}
\item{epsilon}{Defaults to \code{1}.}
\item{threshold_noise}{Defaults to \code{0}.}
\item{query_noise}{Optional; may be \code{NULL}. Passed to \code{.morie_spx_chkv}.}
}
\description{
A step of the sp_fill implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{above}, \code{released}, \code{halted_at}, \code{n_above}, \code{noisy_threshold}, \code{noise_scales}, \code{epsilon_split}, \code{epsilon}, \code{c}, \code{cost_scales_with_c_not_with_m}, \code{answered}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
SparseVector(queries = c(1, 2, 3, 4, 5, 6, 7, 8), threshold = 0.5)
}
