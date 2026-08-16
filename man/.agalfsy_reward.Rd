% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.agalfsy_reward}
\alias{.agalfsy_reward}
\title{.agalfsy_reward}
\usage{
.agalfsy_reward(site, before, after)
}
\arguments{
\item{site}{Passed to \code{.agalfsy_rmsd}.}
\item{before}{Passed to \code{.agalfsy_rmsd}.}
\item{after}{Passed to \code{.agalfsy_rmsd}.}
}
\description{
A step of the agalfsy_native implementation. Called by \code{morie_agalfsy_rl_pose_search}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.agalfsy_reward(site = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), before = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), after = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
