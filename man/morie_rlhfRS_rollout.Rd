% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rlhfRS_rollout}
\alias{morie_rlhfRS_rollout}
\title{Run the policy in a tabular session environment}
\usage{
morie_rlhfRS_rollout(env, policy, n_episodes = 20L, horizon = 10L, gamma = 0.9, seed = 0)
}
\arguments{
\item{env}{The environment.}
\item{policy}{A matrix of action probabilities per state.}
\item{n_episodes}{How many sessions.}
\item{horizon}{How many steps each.}
\item{gamma}{The discount.}
\item{seed}{The random stream.}
}
\description{
The environment gives the states, the actions, the transition probabilities and the expected reward of each state-action pair; the policy gives the probability of each action in each state. Both are indexed by position, so the environment and the policy cannot silently disagree about which action is which. The return is discounted, because a session is not infinite and a click now is worth more than a click after the user has left.
}
\value{
A list with the returns, their mean and standard error, and   the visitation counts.
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
