% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_airl}
\alias{morie_airl}
\title{Adversarial Inverse Reinforcement Learning}
\usage{
morie_airl(expert_states, expert_actions, expert_next, expert_log_policy, 
    policy_states, policy_actions, policy_next, policy_log_policy, gamma = 0.99, 
    state_only = TRUE, lr = 0.1, epochs = 500L, l2 = 0) 
}
\arguments{
\item{expert_states,expert_actions,expert_next}{Expert transitions   \code{(s, a, s\')}. States may be numeric (matrix / vector / list),   strings, or integers; actions may be any value.}
\item{expert_log_policy}{\code{log pi(a | s)} under the CURRENT   policy for each expert transition. AIRL\'s discriminator is   explicitly a function of \code{pi}, so this is not optional   bookkeeping.}
\item{policy_states,policy_actions,policy_next}{Transitions sampled   from the current policy.}
\item{policy_log_policy}{\code{log pi(a | s)} under the current   policy for each policy transition.}
\item{gamma}{Discount used in the shaping term of eq. 4.}
\item{state_only}{If \code{TRUE} (default, Theorem C.1) parameterise   \code{g} on the state alone; if \code{FALSE} (eq. 4 as written)   parameterise on \code{(s, a)}.}
\item{lr}{Learning rate for full-batch gradient ascent.}
\item{epochs}{Number of gradient-ascent iterations.}
\item{l2}{Optional ridge penalty on \code{g} and \code{h}.}
}
\description{
Fits the AIRL discriminator (Fu, Luo & Levine 2018, eq. 4) on expert and policy transitions and reads off the recovered reward \code{log D - log(1 - D)} for each policy transition (line 6 of Algorithm 1). The discriminator \code{D = exp(f) / (exp(f) + pi)} with \code{f = g(s) + gamma h(s\') - h(s)} separates expert (\code{D -> 1}) from policy (\code{D -> 0}) data. \code{state_only = TRUE} (the default, Theorem C.1) parameterises \code{g} on the state alone so the recovered \code{g*} is the ground truth reward up to a constant. \code{state_only = FALSE} uses the \code{g(s, a)} of eq. 4 as written, which fits at least as well but recovers the advantage rather than a transferable reward. AIRL has no random draws: the result is fully determined by the inputs, so no RNG is consulted and \code{set.seed()} is neither needed nor called.
}
\value{
A named list with \code{estimate}, \code{reward}, \code{g},   \code{h}, \code{f_policy}, \code{f_expert}, \code{D_policy},   \code{D_expert}, \code{accuracy}, \code{log_likelihood},   \code{gamma}, \code{state_only}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
r <- morie_airl(expert_states = rnorm(10), expert_actions = rnorm(10), expert_next = rnorm(10),
  expert_log_policy = rnorm(10), policy_states = rnorm(10), policy_actions = rnorm(10),
  policy_next = rnorm(10), policy_log_policy = rnorm(10))
TRUE
}
