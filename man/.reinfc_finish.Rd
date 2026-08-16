% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reinfc_finish}
\alias{.reinfc_finish}
\title{.reinfc_finish}
\usage{
.reinfc_finish(param, rewards, bs, traj)
}
\arguments{
\item{param}{Coerced to list by the body, with \code{as.list}.}
\item{rewards}{A vector; its length is taken and its elements indexed.}
\item{bs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{traj}{Carried through into a list the body builds.}
}
\description{
A step of the reinfc_native implementation. Called by \code{.reinfc_run_bernoulli}, \code{.reinfc_run_gaussian}, \code{.reinfc_run_logistic}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{rewards}, \code{baseline}, \code{trajectory}, \code{n_trials}, \code{mean_reward_first}, \code{mean_reward_last}, \code{method}.
}
\keyword{internal}
\examples{
.reinfc_finish(param = c(1, 2, 3, 4, 5, 6, 7, 8), rewards = c(1, 2, 3, 4, 5, 6, 7, 8), bs = c(1, 2, 3, 4, 5, 6, 7, 8), traj = c(1, 2, 3, 4, 5, 6, 7, 8))
}
