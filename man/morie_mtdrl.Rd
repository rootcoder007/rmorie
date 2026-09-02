% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mtdrl}
\alias{morie_mtdrl}
\title{Run the meta-RL evaluation loop}
\usage{
morie_mtdrl(tasks, agent, episode_length = 100, n_arms = NULL, seed = 0, 
    reset_between_episodes = TRUE) 
}
\arguments{
\item{tasks}{List of per-task probability vectors.}
\item{agent}{A list with reset(), act(features, e), observe(a, r).}
\item{episode_length}{Steps per episode.}
\item{n_arms}{Optional, inferred from tasks.}
\item{seed}{Seed.}
\item{reset_between_episodes}{Apply the per-episode reset.}
}
\description{
Run the meta-RL evaluation loop
}
\value{
A list with mean_reward, total_reward, regret,   reward_by_step, optimal_action_rate, episode_reward, n_episodes,   episode_length, n_arms, method.
}
\keyword{internal}
