% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RespFeat}
\alias{RespFeat}
\title{RespFeat: respiratory rate, depth and I:E ratio}
\usage{
RespFeat(resp, fs, signal_type = "flow", min_breath_s = 1)
}
\arguments{
\item{resp}{See Usage.}
\item{fs}{See Usage.}
\item{signal_type}{Defaults to \code{"flow"}.}
\item{min_breath_s}{Defaults to \code{1}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{rate_breaths_per_min}, \code{mean_period_s}, \code{sd_period_s}, \code{regularity_cv}, \code{depth}, \code{mean_ti_s}, \code{mean_te_s}, \code{ie_ratio}, \code{breaths}, \code{n_breaths}, \code{signal_type}, \code{fs_hz}, \code{units}, \code{method}.
}
\keyword{internal}
