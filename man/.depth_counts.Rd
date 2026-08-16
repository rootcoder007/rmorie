% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.depth_counts}
\alias{.depth_counts}
\title{Split counts by (depth, feature), depth 1 at the root. Mirrors}
\usage{
.depth_counts(tree, max_depth, d)
}
\arguments{
\item{tree}{See Usage.}
\item{max_depth}{A count; the body uses it as \code{seq_len(...)}.}
\item{d}{A count; the body uses it as \code{numeric(...)}.}
}
\description{
morie.fn.crfsel._depth_counts; element [[depth]] holds the counts at that depth, so the caller indexes by depth directly.
}
\value{
The value of \code{counts}, as built in the body.
}
\keyword{internal}
