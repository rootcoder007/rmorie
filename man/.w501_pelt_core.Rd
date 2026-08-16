% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w501_pelt_core}
\alias{.w501_pelt_core}
\title{.w501_pelt_core}
\usage{
.w501_pelt_core(x, cost, penalty, min_seglen = 1L)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{cost}{Passed to \code{.w501_seg_cost}.}
\item{penalty}{See Usage.}
\item{min_seglen}{Passed to \code{seq}. Defaults to \code{1L}.}
}
\description{
Part of the w501_changepoint_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{taus}, \code{Fn}.
}
\keyword{internal}
