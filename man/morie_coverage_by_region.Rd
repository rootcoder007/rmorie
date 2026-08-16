% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_coverage_by_region}
\alias{morie_coverage_by_region}
\title{morie_coverage_by_region}
\usage{
morie_coverage_by_region(theta, level = 0.95, lower_bound = 0, draws = 20000L, seed = 0, split = NULL)
}
\arguments{
\item{theta}{See Usage.}
\item{level}{Defaults to \code{0.95}.}
\item{lower_bound}{Defaults to \code{0}.}
\item{draws}{Defaults to \code{20000L}.}
\item{seed}{Defaults to \code{0}.}
\item{split}{Defaults to \code{NULL}.}
}
\description{
Part of the bndpcb_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{marginal_coverage}, \code{subset_coverage}, \code{subset_share}, \code{mean_width}, \code{p_empty}, \code{split}, \code{theta}, \code{draws}.
}
\keyword{internal}
