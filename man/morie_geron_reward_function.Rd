% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_reward_function}
\alias{morie_geron_reward_function}
\title{Reward function R(s, a, s\'): per-transition rewards and the backward discounted return (Geron Ch 19, hmrwd)}
\usage{
morie_geron_reward_function(s, a, s_next, R = NULL, gamma = 1)
}
\arguments{
\item{s,a,s_next}{Transition or trajectory arrays.}
\item{R}{Callable `R(s,a,s\')` or a 2-D/3-D lookup table.}
\item{gamma}{Discount in \[0, 1\].}
}
\description{
Reward function R(s, a, s\'): per-transition rewards and the backward discounted return (Geron Ch 19, hmrwd)
}
\value{
A list with \code{rewards}, \code{total_reward}, \code{returns}, \code{discounted_return}, \code{gamma}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
