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
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{n_subsets}{See Usage.}
\item{seed}{See Usage.}
}
\description{
local IRLS refinement. Non-convex objective, so the subsets are the global search and the refinement the local one.
}
\value{
A list with \code{beta}, \code{scale}.
}
\keyword{internal}
