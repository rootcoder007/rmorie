% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.split_frequency_importance}
\alias{.split_frequency_importance}
\title{Depth-weighted split frequency}
\usage{
.split_frequency_importance(trees, d, max_depth, decay)
}
\arguments{
\item{trees}{A list of tree objects.}
\item{d}{Integer number of covariates.}
\item{max_depth}{Integer, depths above this are dropped.}
\item{decay}{Numeric, depth decay.}
}
\description{
Mirrors \code{split_frequency_importance}: counts the splits at each depth WITHIN that depth, then weights by \code{1 / depth^decay}, and normalises the result to sum to one.
}
\value{
Numeric vector of length d summing to one.
}
\keyword{internal}
