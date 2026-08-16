% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rob_s_reg}
\alias{.rob_s_reg}
\title{S-estimator core: random p-subsets ranked by residual M-scale, then}
\usage{
.rob_s_reg(X, y, n_subsets, seed)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{n_subsets}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Passed to \code{set.seed}.}
}
\description{
local IRLS refinement. Non-convex objective, so the subsets are the global search and the refinement the local one.
}
\value{
A list with \code{beta}, \code{scale}.
}
\keyword{internal}
