% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_timesfm_rollout_steps}
\alias{morie_timesfm_rollout_steps}
\title{morie_timesfm_rollout_steps}
\usage{
morie_timesfm_rollout_steps(horizon, output_patch_len)
}
\arguments{
\item{horizon}{See Usage.}
\item{output_patch_len}{See Usage.}
}
\description{
Part of the timesfm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{steps}, \code{horizon}, \code{output_patch_len}, \code{single_step}.
}
\keyword{internal}
\examples{
morie_timesfm_rollout_steps(horizon = 5L, output_patch_len = 5L)
}
