% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rlhfRS_offpolicy}
\alias{morie_rlhfRS_offpolicy}
\title{Estimate a policy's value from another policy's log}
\usage{
morie_rlhfRS_offpolicy(log, policy, behaviour, estimator = "ips", reward_model = NULL, clip = NULL)
}
\arguments{
\item{log}{The logged decisions.}
\item{policy}{The target policy.}
\item{behaviour}{The logging propensities.}
\item{estimator}{One of ips, snips or dr.}
\item{reward_model}{A per-state-action reward model, for dr.}
\item{clip}{A cap on the importance weight, or NULL.}
}
\description{
The log is a list of state, action and reward triples as they were actually observed. The behaviour vector is the probability the logging policy assigned to the action it took -- the propensity -- and it has to come from the logging system, not be re-estimated here, or the correction corrects for the wrong thing. The clip caps the importance weight. It trades unbiasedness for a bounded variance and it is off by default, because a silently clipped estimate is a biased estimate that looks like an unbiased one; the number of weights it bit is reported.
}
\value{
A list with the estimate, the weights and the diagnostics.
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
