% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_reinforce}
\alias{morie_geron_reinforce}
\title{REINFORCE ascent step: gradient from morie_geron_policy_gradient, applied to theta (Geron Ch 19, hmrnfc)}
\usage{
morie_geron_reinforce(episodes, policy, gamma = 0.99, eta = 0.01, theta = NULL, baseline = TRUE) 
}
\arguments{
\item{episodes}{List of episodes.}
\item{policy}{Function `policy(state, action) -> grad log pi`.}
\item{gamma,eta,theta,baseline}{Controls.}
}
\description{
REINFORCE ascent step: gradient from morie_geron_policy_gradient, applied to theta (Geron Ch 19, hmrnfc)
}
\value{
A list with \code{theta}, \code{theta_next}, \code{step}, \code{gradient}, \code{returns}, \code{mean_return}, \code{baseline_value}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
gs <- function(s, a) if (a == 0) c(1, 0) else c(0, 1)
morie_geron_reinforce(list(list(list(0, 0, 1), list(1, 1, 1))), 
    gs, gamma = 0.5, eta = 0.1)
}
