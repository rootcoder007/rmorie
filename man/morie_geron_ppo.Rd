% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_ppo}
\alias{morie_geron_ppo}
\title{PPO clipped-surrogate policy optimisation over a tabular softmax policy (Geron Ch 19, hmppo)}
\usage{
morie_geron_ppo(env, policy, epochs = 20, lr = 0.1, clip_eps = 0.2, gamma = 0.99, 
    n_episodes = 8, max_steps = 50, n_updates = 4, seed = 0) 
}
\arguments{
\item{env}{List with `reset()` / `step(a)`.}
\item{policy}{Initial (n_states, n_actions) logits.}
\item{epochs,lr,clip_eps,gamma,n_episodes,max_steps,n_updates,seed}{Training controls.}
}
\description{
PPO clipped-surrogate policy optimisation over a tabular softmax policy (Geron Ch 19, hmppo)
}
\value{
A list with \code{theta}, \code{probabilities}, \code{return_history}, \code{surrogate_history}, \code{clip_fraction}, \code{clip_eps}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
reset <- function() 0
step <- function(a) list(0, as.numeric(a), TRUE)
morie_geron_ppo(list(reset = reset, step = step), matrix(c(0, 
    0), nrow = 1), epochs = 30, lr = 0.5, seed = 1)
}
