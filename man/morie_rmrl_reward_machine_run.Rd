% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rmrl_reward_machine_run}
\alias{morie_rmrl_reward_machine_run}
\title{Drive a machine over a sequence of truth assignments. labels is}
\usage{
morie_rmrl_reward_machine_run(machine, labels)
}
\arguments{
\item{machine}{See Usage.}
\item{labels}{See Usage.}
}
\description{
sigma_0, sigma_1, ..., i.e. L(s) for each visited state. Returns the machine-state trajectory and the rewards emitted.
}
\value{
A list with \code{estimate}, \code{states}, \code{rewards}, \code{total_reward}, \code{final_state}, \code{accepted}, \code{method}.
}
\keyword{internal}
