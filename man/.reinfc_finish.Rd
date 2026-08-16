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
\item{param}{See Usage.}
\item{rewards}{A vector; its length is taken and its elements indexed.}
\item{bs}{See Usage.}
\item{traj}{See Usage.}
}
\description{
A step of the reinfc_native implementation. Called by \code{.reinfc_run_bernoulli}, \code{.reinfc_run_gaussian}, \code{.reinfc_run_logistic}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{rewards}, \code{baseline}, \code{trajectory}, \code{n_trials}, \code{mean_reward_first}, \code{mean_reward_last}, \code{method}.
}
\keyword{internal}
