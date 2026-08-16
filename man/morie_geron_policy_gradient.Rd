% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_policy_gradient}
\alias{morie_geron_policy_gradient}
\title{Policy gradient (REINFORCE) estimator over discounted returns (Geron Ch 19, hmpg)}
\usage{
morie_geron_policy_gradient(trajectories, policy, gamma = 0.99, baseline = FALSE)
}
\arguments{
\item{trajectories}{List of episodes, each a list of `(state, action, reward)`.}
\item{policy}{Function `policy(state, action) -> grad log pi` (or `(logp, grad)`).}
\item{gamma}{Discount.}
\item{baseline}{Subtract the mean return.}
}
\description{
Policy gradient (REINFORCE) estimator over discounted returns (Geron Ch 19, hmpg)
}
\value{
A list with \code{gradient}, \code{returns}, \code{mean_return}, \code{n_steps}, \code{n_episodes}, \code{baseline_value}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
