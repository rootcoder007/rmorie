% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_reinforcement_learning}
\alias{morie_geron_reinforcement_learning}
\title{Reinforcement learning: Monte-Carlo policy evaluation by rollout (Geron Ch 1, hmrl)}
\usage{
morie_geron_reinforcement_learning(env, pi, gamma = 0.99, n_episodes = 1, max_steps = 1000, seed = 0) 
}
\arguments{
\item{env}{List with `reset()` / `step(action)`.}
\item{pi}{Function `pi(state) -> action or probs`.}
\item{gamma,n_episodes,max_steps,seed}{Controls.}
}
\description{
Reinforcement learning: Monte-Carlo policy evaluation by rollout (Geron Ch 1, hmrl)
}
\value{
A list with \code{mean_return}, \code{returns}, \code{lengths}, \code{se}, \code{effective_horizon}, \code{truncated}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
reset <- function() 0
step <- function(a) list(0, as.numeric(a), TRUE)
morie_geron_reinforcement_learning(list(reset = reset, step = step), 
    function(s) 0, gamma = 0.5)
}
