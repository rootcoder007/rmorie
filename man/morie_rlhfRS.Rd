% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rlhfRS}
\alias{morie_rlhfRS}
\title{Evaluate a recommendation policy, on-policy or from a log}
\usage{
morie_rlhfRS(env, policy, n_episodes = 20L, horizon = 10L, gamma = 0.9, seed = 0, 
    estimator = "ips", reward_model = NULL, clip = NULL, relevance = NULL, 
    k = 5L) 
}
\arguments{
\item{env}{The environment.}
\item{policy}{A matrix of action probabilities per state.}
\item{n_episodes,horizon,gamma,seed}{The rollout settings.}
\item{estimator}{Which off-policy estimator.}
\item{reward_model}{A per-state-action reward model, for dr.}
\item{clip}{A cap on the importance weight, or NULL.}
\item{relevance}{A ranked relevance vector, or NULL.}
\item{k}{The ranking cut-off.}
}
\description{
An environment carrying transition and reward is simulated. One carrying log and behaviour is evaluated off-policy against those logged decisions. Both may be present, and then both are reported -- which is the useful case, because the gap between the simulated value and the off-policy estimate is the thing worth looking at.
}
\value{
A list with the value estimate or estimates, the visitation,   the ranking metrics, and the off-policy diagnostics.
}
\references{
  Zhao, X., Zhang, L., Ding, Z., Xia, L., Tang, J. and Yin, D. (2018)
    "Recommendations with negative feedback via pairwise deep
    reinforcement learning." KDD, 1040-1048.
    doi:10.1145/3219819.3219886.
  Horvitz, D.G. and Thompson, D.J. (1952) "A generalization of
    sampling without replacement from a finite universe." JASA
    47(260), 663-685.
  Swaminathan, A. and Joachims, T. (2015) "The self-normalized
    estimator for counterfactual learning." NeurIPS 28, 3231-3239.
  Dudik, M., Langford, J. and Li, L. (2011) "Doubly robust policy
    evaluation and learning." ICML, 1097-1104.
  Jarvelin, K. and Kekalainen, J. (2002) "Cumulated gain-based
    evaluation of IR techniques." ACM TOIS 20(4), 422-446.
}
\keyword{internal}
\examples{
P <- list(list(c(0.7, 0.3), c(0.5, 0.5), c(0.2, 0.8)), list(c(0.6, 
    0.4), c(0.3, 0.7), c(0.9, 0.1)))
R <- list(c(0.1, 0.4, 1), c(1.2, 0.3, 0.2))
ENV <- list(transition = P, reward = R, start = 0L)
GREEDY <- matrix(c(0, 1, 0, 0, 1, 0), 2L, 3L)
morie_rlhfRS(ENV, GREEDY, 25L, 8L, 0.9, 7)
}
